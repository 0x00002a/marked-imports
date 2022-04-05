{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Lib
    ( run, runWithCtx, mkPkgLookupCtx, mkAndPopulateStackDb, parseToAST,
    unAST, stripPackageComments, runT, sortImportsOn, addLinesBeforeGroups, ProcessedNode(..), locationSum, runWithCtxT
    , stripWhitespaceBetweenImports
    ) where

import           Control.Arrow      ( first, second )
import           Control.Monad      ( foldM, join )
import           Control.Monad.Cont ( liftIO )
import           Data.Bifunctor     ( Bifunctor (bimap) )
import           Data.Foldable      ( foldlM, toList )
import           Data.List          ( find, sort, sortOn )
import           Data.Map           ( Map )
import qualified Data.Map           as M
import           Data.Maybe         ( catMaybes, fromJust, fromMaybe, isJust, isNothing )
import qualified Data.Set           as Set
import           Data.Text          ( Text, pack, unpack )
import qualified Data.Text          as TxT
import           Debug.Trace        ( traceShow, traceShowId )
import qualified GhcPkg             as GPKG
import qualified LUtil              as Util
import qualified Packages           as PKG
import qualified Parser             as P
import           System.Exit        ( ExitCode (..) )
import qualified System.Process     as SP
import qualified Text.Megaparsec    as MP
import qualified Types              as T

newtype GhcPkgDbSource db = GhcPkgDbSource db

data ProcessedNode = PRawLine (T.Located Text) | POldImportCmt (T.Located Text) | PImportGroup (T.Located T.PackageInfo) [T.Located T.ImportDecl] deriving (Show, Eq)
type ProcessedAST = [ProcessedNode]

type ProcessErr = (Text, T.Located T.ImportDecl)

instance (GPKG.Database db) => PKG.MappingSource (GhcPkgDbSource db) where
    providerOfModule (GhcPkgDbSource db) name = pure $
            maybe (Left "could not find package") Right $ T.pkgInfo <$> GPKG.lookup db name

run :: T.SourceInfo Text -> IO (Text, Text)
run src = runT src id

runT :: T.SourceInfo Text -> (ProcessedAST -> ProcessedAST) -> IO (Text, Text)
runT src t = do
    ctx <- mkPkgLookupCtx
    runWithCtxT ctx src t

parseToAST :: PKG.MappingSource s => s -> T.SourceInfo Text -> IO (T.Result (ProcessedAST, [ProcessErr]))
parseToAST pkgCtx (T.SourceInfo name content) = do
    case MP.parse P.parseFile (unpack name) content of
        Left err -> pure $ T.err $ (("parse error: " <>) . pack . MP.errorBundlePretty) err
        Right rs -> T.ok <$> modifyContent pkgCtx content rs

errorPretty :: ProcessErr -> Text
errorPretty (msg, loc) =
    (TxT.pack . show . T.srcLine $ T.posOf loc)
    <> ": "
    <> snd (T.unLocated loc)
    <> "\nerror: "
    <> msg

unLoc :: ProcessedNode -> T.Pos
unLoc (PImportGroup n _) = T.posOf n
unLoc (POldImportCmt n)  = T.posOf n
unLoc (PRawLine n)       = T.posOf n

setLoc :: ProcessedNode -> T.Pos -> ProcessedNode
setLoc (PImportGroup n i) p = PImportGroup (T.Located p (T.unLocated n)) i
setLoc (POldImportCmt n) p  = POldImportCmt (T.Located p (T.unLocated n))
setLoc (PRawLine n) p       = PRawLine (T.Located p (T.unLocated n))

sortOnPos = sortOn unLoc

offsetBy :: T.Pos -> ProcessedNode -> ProcessedNode
offsetBy p n = setLoc n (unLoc n + p)

stripWhitespaceBetweenImports :: ProcessedAST -> ProcessedAST
stripWhitespaceBetweenImports xs = filter (\x -> isNothing (find (== x) stripped)) xs
    where
        imports = concatMap (\(PImportGroup _ imps) -> imps) $ filter filterByImpGroup xs
        importLines = map T.posOf imports
        minmax = case importLines of
            [] -> Nothing
            xs -> Just (minimum xs, maximum xs)
        inStripZone x = (\(min, max) -> T.posOf x >= min && T.posOf x <= max) <$> minmax
        filterByStrip = filter (\(PRawLine x) -> fromMaybe False (inStripZone x) && T.unLocated x == "")
        filterByLine = filter isRawLine
        stripped = (filterByStrip . filterByLine) xs

addLinesBeforeGroups :: Int -> ProcessedAST -> ProcessedAST
addLinesBeforeGroups lines = foldl doMap mempty . sortOnPos
    where
        doMap xs v@(PImportGroup _ _) = setLoc v (curr + T.Pos lines):genLines curr <> xs
            where
                curr = locationSum xs + 1
        doMap xs x = offsetBy (T.Pos offset) x:xs
            where
                offset = lines * (length $ filter filterByImpGroup xs)
        genLines start = foldl (\xs n -> PRawLine (T.Located n ""):xs) mempty [start .. (start + T.Pos lines - 1)]

filterByImpGroup (PImportGroup _ _) = True
filterByImpGroup _                  = False
isRawLine (PRawLine _) = True
isRawLine _ = False

sortImportsOn :: (Num n, Ord n) => (T.PackageInfo -> n) -> ProcessedAST -> ProcessedAST
sortImportsOn f ast = sortedPkgs <> map fst nonPkgs
    where
        extractPackages = map (\x -> (x, unLoc x)) ast
        sortedPkgs = applyIndex . sortOn (\(PImportGroup n _, _) -> f (T.unLocated n)) $ filter doFilter extractPackages
        applyIndex xs = zipWith setLoc pkgs positions
            where
                pkgs = map fst xs
                positions = sort (map snd xs)
        nonPkgs = filter (not . doFilter) extractPackages
        doFilter (PImportGroup _ _, _) = True
        doFilter _                     = False

stripPackageInfo :: ProcessedAST -> ProcessedAST
stripPackageInfo = concatMap doStrip
    where
        sortFn (POldImportCmt line) = T.posOf line
        sortFn (PRawLine line)      = T.posOf line
        doStrip :: ProcessedNode -> [ProcessedNode]
        doStrip (PImportGroup info imports) = map (PRawLine . fmap snd) imports
        doStrip x                           = [x]

stripPackageComments :: ProcessedAST -> ProcessedAST
stripPackageComments = stripPackageInfo . filter notOldCmt
    where
        notOldCmt (POldImportCmt _) = False
        notOldCmt _                 = True

linesPreserve ""  = []
linesPreserve txt = TxT.split (=='\n') txt

unlinesPreserve []     = ""
unlinesPreserve [x]    = x
--unlinesPreserve (x:"":xs) = x <> "\n" <> unlinesPreserve xs
unlinesPreserve (x:xs) = x <> "\n" <> unlinesPreserve xs

cmtToTxt (T.SingleLineCmt "") = ""
cmtToTxt (T.SingleLineCmt c)  = "-- " <> c

unAST :: ProcessedAST -> Text
unAST = unlinesPreserve . map unpackAST . sortByPos . stripOld
    where
        sortByPos = sortOn unLoc
        unpackAST (PRawLine l) = T.unLocated l
        unpackAST (PImportGroup pkg imports) = unlinesPreserve $ cmtToTxt (packageToComment (T.unLocated pkg)):map (snd . T.unLocated) imports
        stripOld = filter (\case
                            POldImportCmt _ -> False
                            _               -> True
                            )


runWithCtxT :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> (ProcessedAST -> ProcessedAST) -> IO (Text, Text)
runWithCtxT mPkgCtx src t =
    either (mempty,) ok . join <$> sequence (flip parseToAST src <$> mPkgCtx)
    where
        ok :: (ProcessedAST, [ProcessErr]) -> (Text, Text)
        ok = bimap (unAST . t) (foldl (<>) mempty . map errorPretty)

runWithCtx :: PKG.MappingSource s => T.Result s -> T.SourceInfo Text -> IO (Text, Text)
runWithCtx mPkgCtx src = runWithCtxT mPkgCtx src id


eqByLine :: T.Pos -> T.Located a -> Bool
eqByLine rx (T.Located lx _) = lx == rx

mkAndPopulateStackDb :: IO (T.Result GPKG.MapStore)
mkAndPopulateStackDb = GPKG.mkDbAndPopulate proc
    where
        proc = GPKG.pkgCmd (uncurry SP.proc)

mkPkgLookupCtx :: IO (T.Result (PKG.LocalPkgMatcher (GhcPkgDbSource GPKG.MapStore)))
mkPkgLookupCtx = do
    db <- ctx
    pure $ PKG.mkLocalMatcher <$> db
    where
        ctx = do
            db <- mkAndPopulateStackDb
            pure $ GhcPkgDbSource <$> db

packageCommentPrefix = "-- "

packageToComment :: T.PackageInfo -> T.Comment
packageToComment pkg = T.SingleLineCmt $ T.pkgName pkg

-- Second result is errors
extractImports ::
    PKG.MappingSource s => s
    -> T.Module
    -> IO (Map T.PackageInfo [T.Located T.ImportDecl], [(Text, T.Located T.ImportDecl)])
extractImports ctx mod = foldlM  doFold (mempty, mempty) (T.modImports mod)
    where
        doFold (xs, errs) name = do
            minfo <- PKG.providerOfModule ctx (fst $ T.unLocated name)
            pure $ case minfo of
                Left err   -> (xs, (err, name):errs)
                Right info -> (M.insert info (name:M.findWithDefault [] info xs) xs, errs)

locationSum :: ProcessedAST -> T.Pos
locationSum [] = T.Pos 0
locationSum xs = maximum $ map doProcess xs
    where
        doProcess (POldImportCmt m)   = T.posOf m
        doProcess (PRawLine l)        = T.posOf l
        doProcess (PImportGroup n im) = T.posOf n + importDiff im
        importDiff im = sum $ map (\i -> T.Pos (maximum (Util.linesCoveredByImport i)) - T.posOf i) im

modifyContent :: PKG.MappingSource s => s -> Text -> T.Module -> IO (ProcessedAST, [(Text, T.Located T.ImportDecl)])
modifyContent mapCtx txt mod = do
    imports <- extractImports mapCtx mod
    let linesOut = Util.foldlWithIndex (foldLines (fst imports))
            mempty
            lines
    pure (linesOut, snd imports)
    where
        lines = linesPreserve txt
        txtForImport imp = lines !! ((T.srcLine $ T.posOf imp) - 1)
        importOnLine line = isJust $ find (==line) importLines
        commentOnLine line = isJust $ find (==line) $ map (T.srcLine . T.posOf) (T.modComments mod)
        linesForComment = filter (not . commentOnLine . (\x -> x - 1)) importLines
        importLines = concatMap Util.linesCoveredByImport $ T.modImports mod
        importsRange = case importLines of
            []    -> Nothing
            lines -> Just (minimum lines, maximum lines)
        toCommentGroup xs (pkg, imports) = xs <> [PImportGroup (T.Located (locationSum xs + 1) pkg) imports]
        oldComments pkgs = filter (`Set.notMember` pkgCmts pkgs) . Set.toList . pkgCmts . M.keysSet
            where
                pkgCmts = Set.map (cmtToTxt . packageToComment)
        --commented :: Map T.PackageInfo [T.Located T.ImportDecl] -> ProcessedAST
        commented start = foldl toCommentGroup start . M.toList
        foldLines :: Map T.PackageInfo [T.Located T.ImportDecl] -> [ProcessedNode] -> Int -> Text -> [ProcessedNode]
        foldLines imports result lineNb line
            | maybe False ((lineNb ==) . fst) importsRange =
                commented result imports

            | commentOnLine lineNb && notPassthroughComment line = result <> [POldImportCmt (T.Located pos line)]
            | importOnLine lineNb = result
            | otherwise = nextV
            where
                pos = T.Pos lineNb
                nextV = result <> [PRawLine $ T.Located pos line]
                allPackages = Set.map packageToComment $ M.keysSet imports
                notPassthroughComment line = Set.member line (Set.map cmtToTxt allPackages)













