{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Arrows #-}
module Marvin.GUI.Main (mainMarvinGUI) where

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Glade

import Data.Maybe
import Data.Traversable
import Control.Arrow

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString as BS

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Concurrent.MVar

import Marvin.API
import Marvin.API.Table.TrainTestSplit
import Marvin.API.Table.PrettyPrint
import Marvin.API.Table.Internal

import Marvin.GUI.AlgorithmDescriptor
import Marvin.GUI.ConfigWindow

import Data.Traversable
import qualified Data.Vector as Vec
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Vector (Vector)
import Data.Tuple (swap)
import qualified Data.Map as Map

type TargetColumnSelector = (ComboBox, ListStore Int, ConnectId ComboBox)
type HeaderWidget = (VBox, CheckButton, ComboBox)

data TestLoader = Split Double | Path FilePath deriving Show

data GUIGlobalState = GUIGlobalState {
  trainFilePath :: Maybe FilePath
  , trainHasHeader :: Bool
  , tableProps :: Maybe TableProperties
  , targetColSelector :: TargetColumnSelector
  , tableBox :: HBox
  , configButton :: Button
  , runButton :: Button
  , headerWidgets :: Maybe (Vector HeaderWidget)
  , selectedAlgorithm :: Maybe AlgorithmDescriptor
  , algorithmParams :: Maybe Parameters
  , selectedTarget :: Maybe Int
  , testLoader :: Maybe TestLoader
  , loadedTable :: Maybe RawTable
} deriving Show

instance (Show a) => Show (ConnectId a) where
  show _ = "<ConnectId>"
instance (Show a) => Show (ListStore a) where
  show _ = "<ListStore>"
instance Show HBox where
  show _ = "<HBox>"
instance Show VBox where
  show _ = "<VBox>"
instance Show CheckButton where
  show _ = "<CheckButton>"
instance Show ComboBox where
  show _ = "<ComboBox>"
instance Show Button where
  show _ = "<Button>"

emptyGUIState tBox targetSelector configButton_ runButton_ testLoader_ = GUIGlobalState {
  trainFilePath = Nothing
  , trainHasHeader = False
  , tableProps = Nothing
  , targetColSelector = targetSelector
  , tableBox = tBox
  , configButton = configButton_
  , runButton = runButton_
  , headerWidgets = Nothing
  , selectedAlgorithm = Nothing
  , algorithmParams = Nothing
  , selectedTarget = Nothing
  , testLoader = testLoader_
  , loadedTable = Nothing
}

-- Table props --
data TableProperties = TableProperties {
  selectedColumns :: Vector Bool
  , selectedColumnTypes :: Vector DataType
  , columnNameStrings :: Vector String
} deriving Show

defaultTableProps :: RawTable -> TableProperties
defaultTableProps t =
  TableProperties {
    selectedColumns = Vec.replicate n True
    , selectedColumnTypes = Vec.fromList types
    , columnNameStrings = names
  }
  where
    n = numberOfColumns t
    genTypeToNumNom Binary = Nominal
    genTypeToNumNom Natural = Numeric
    genTypeToNumNom x = x
    types = fmap genTypeToNumNom $ columnTypes $ smartParseTable t
    names = Vec.fromList $ zipWith (\i n -> n ++ " (" ++ show i ++ ")") [0..] $
      fmap (either (const "") id) $ columnNames t

updateTableProps :: (TableProperties -> TableProperties) -> GUIGlobalState -> GUIGlobalState
updateTableProps f gui = gui { tableProps = fmap f (tableProps gui) }

parseSelectedColumnsWithTarget :: Int -> RawTable -> TableProperties ->
  Fallible (DataTable, DataColumn)
parseSelectedColumnsWithTarget targetIdx textTab tab = do
  xParsed <- parseWithScheme (Vec.toList xTypes) xText
  yParsed <- parseColumnAs yType yText
  return (xParsed, yParsed)
  where
    -- text szetszed, utana parse
    colTypes = selectedColumnTypes tab
    -- x
    allIndices = Vec.fromList [0..(numberOfColumns textTab - 1)]
    selectedColIndices = List.delete targetIdx $
      map fst $ filter snd $ zip (Vec.toList allIndices) $ Vec.toList $ selectedColumns tab
    xTypes = Vec.fromList $ map (\i -> colTypes Vec.! i) selectedColIndices
    xText = unsafeSelectColumns textTab selectedColIndices
    -- y
    yType = colTypes Vec.! targetIdx
    yText = columnsVec textTab Vec.! targetIdx


type GUIState a = StateT GUIGlobalState IO a
type GUIError a = Either String a

getsGUI :: (GUIGlobalState -> a) -> GUIMVar a
getsGUI f = do
  x <- asks $ \gui -> withMVar gui (return . f)
  lift x

modifyGUI :: (GUIGlobalState -> GUIGlobalState) -> GUIMVar ()
modifyGUI f = do
  x <- asks $ \gui -> modifyMVar_ gui (return . f)
  lift x

type GUIMVar a = ReaderT (MVar GUIGlobalState) IO a

runGUIState' :: MVar GUIGlobalState -> GUIState a -> IO a
runGUIState' mvar s = modifyMVar mvar (fmap swap . runStateT s)

runGUIMVar' :: MVar GUIGlobalState -> GUIMVar a -> IO a
runGUIMVar' guiMVar f = (runReaderT f) guiMVar

runGUIStateWithMVar :: GUIState a -> GUIMVar a
runGUIStateWithMVar guiState = do
  result <- asks (\guiMVar -> runGUIState' guiMVar guiState)
  lift result

mainMarvinGUI = do
  -- create GUI
  initGUI
  Just xml <- xmlNew "gui_main.glade"
  -- widgets
  window <- xmlGetWidget xml castToWindow "window_main"
  windowSetTitle window "Marvin GUI"
  fileChooserTrain <- xmlGetWidget xml castToFileChooserButton "file_chooser_train"
  widgetSetTooltipText fileChooserTrain $ Just $
    "Choose a file for training data."
  buttonLoadTrain <- xmlGetWidget xml castToButton "button_load_train"
  widgetSetTooltipText buttonLoadTrain $ Just $
    "Load training data from comma separated file. " ++
    "A file must be chosen to use this."
  checkButtonHeader <- xmlGetWidget xml castToCheckButton "checkbutton_header"
  vBoxAlgorithm <- xmlGetWidget xml castToVBox "vbox_algorithm"

  -- table space
  fixedTableSpace <- xmlGetWidget xml castToFixed "fixed_table"
  tableHBox <- hBoxNew False 0
  fixedPut fixedTableSpace tableHBox (0, 0)

--   fixedTableSpace <- xmlGetWidget xml castToScrolledWindow "scrolledwindow"
--   tableHBox <- hBoxNew False 0
--   scrolledWindowPut fixedTableSpace tableHBox

  -- target column selector
  fixedTargetSelector <- xmlGetWidget xml castToFixed "fixed_target_column_selector"
  (combo, model, handler) <- mkTargetColumnSelector
  fixedPut fixedTargetSelector combo (0, 0)
  widgetSetTooltipText fixedTargetSelector $ Just $
    "Select a target variable to use in supervised learning. " ++
    "A table must be loaded with selected columns to use this."

  -- algo selector fixed
  fixedAlgorithmSelector <- xmlGetWidget xml castToFixed "fixed_algorithm_selector"
  widgetSetTooltipText fixedAlgorithmSelector $ Just
    "Select an algorithm."

  -- algo config button
  buttonConfigAlgorithm <- xmlGetWidget xml castToButton "button_config_algorithm"
  widgetSetTooltipText buttonConfigAlgorithm $ Just
    "Configure algorithm parameters. An algorithm must be chosen first to use this."

  -- algo run button
  buttonRunAlgorithm <- xmlGetWidget xml castToButton "button_run_algorithm"
  widgetSetTooltipText buttonRunAlgorithm $ Just $
    "Run algorithm. To use this, an algorithm must be chosen, " ++
    "a table must be loaded with at least 2 selected columns, and a target variable must be chosen."

  -- output textview
  textViewOutput <- xmlGetWidget xml castToTextView "textview_output"
  outputBuffer <- textBufferNew Nothing
  textViewSetBuffer textViewOutput outputBuffer

  -- default test loader
  let defaultSplit = 0.8
  let defaultTestLoader = Just $ Split defaultSplit

  -- init state of load page
  guiState <- newMVar $ emptyGUIState tableHBox (combo, model, handler)
    buttonConfigAlgorithm buttonRunAlgorithm defaultTestLoader
  let runGUIState = runGUIState' guiState
  let runGUIMVar = runGUIMVar' guiState
  -- widget events
  on fileChooserTrain fileChooserButtonFileSet $
    runGUIState $ onTrainFileSet fileChooserTrain buttonLoadTrain
  onClicked buttonLoadTrain $ runGUIMVar $ do
      onTableLoadButtonClicked checkButtonHeader
      registerColumnSelectionListeners
  onClicked buttonConfigAlgorithm $
    runGUIState onConfigButtonClicked

  -- algorithm run
  onClicked buttonRunAlgorithm $ do
    fallibleResult <- runGUIState onAlgorithmRun
    case fallibleResult of
          Right output -> textBufferSetText outputBuffer output
          Left e -> runErrorDialog e


  -- test loader
  mkTestLoader guiState xml defaultSplit

  -- algo selector
  algoSelector <- mkAlgorithmSelector guiState algorithms
  fixedPut fixedAlgorithmSelector algoSelector (0, 0)

  -- destroy GUI
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

onAlgorithmRun :: GUIState (Fallible String)
onAlgorithmRun = do
  Just algo <- gets selectedAlgorithm
  Just targetIdx <- gets selectedTarget
  Just tabProps <- gets tableProps
  Just params <- gets algorithmParams
  Just testLoader <- gets testLoader
  Just loadedTab <- gets loadedTable
  split <- loadTrainTest loadedTab testLoader
  let pipe = proc table -> do
        (x, y) <- inject -< parseSelectedColumnsWithTarget targetIdx table tabProps
        output <- runAlgorithm algo params -< (x, y)
        returnA -< output
  let fallibleResult = do
        (train, test) <- split
        (fitThenRun pipe) train test
  return fallibleResult

runErrorDialog error = do
  dialog <- dialogNew
  windowSetTitle dialog "ERROR"
  windowSetModal dialog True
  windowSetDeletable dialog False
  -- button
  dialogAddButton dialog "OK" ResponseOk

  vbox <- dialogGetUpper dialog
  errorLabel <- labelNew $ Just $ show error
  boxPackStart vbox errorLabel PackNatural 0

  onDestroy dialog $ dialogResponse dialog ResponseDeleteEvent
  widgetShowAll dialog

  response <- dialogRun dialog
  widgetDestroy dialog

loadTrainTest :: RawTable -> TestLoader -> GUIState (Fallible (RawTable, RawTable))
loadTrainTest table (Split ratio) = do
  split <- lift $ trainTestSplit ratio table
  return split
loadTrainTest train (Path testPath) = do
  header <- gets trainHasHeader
  let hasHeader = case header of
        True -> HasHeader
        False -> NoHeader
  test <- lift $ fromCsv ','  hasHeader testPath
  return $ (\t -> (train, t)) <$> test


mkTestLoader :: MVar GUIGlobalState -> GladeXML -> Double -> IO ()
mkTestLoader guiState xml defaultSplit = do
  let runGUIMVar = runGUIMVar' guiState
  -- spin
  radioButtonSplit <- xmlGetWidget xml castToRadioButton "radiobutton_test_split"
  widgetSetTooltipText radioButtonSplit  $ Just $
    "Split data randomly to provide a training and test datasets."

  radioButtonLoad <- xmlGetWidget xml castToRadioButton "radiobutton_test_load"
  widgetSetTooltipText radioButtonLoad $ Just $
    "Load a file for test data separately from training data."

  fileChooserTest <- xmlGetWidget xml castToFileChooserButton "file_chooser_test"
  widgetSetTooltipText fileChooserTest  $ Just $
    "Load a file for test data separately from training data."

  fixedTestSplit <- xmlGetWidget xml castToFixed "fixed_test_split"
  widgetSetTooltipText fixedTestSplit  $ Just $
    "Set the ratio of training and test data."

  spinButton <- spinButtonNewWithRange 0 1 0.05
  spinButtonSetValue spinButton defaultSplit
  fixedPut fixedTestSplit spinButton (0, 0)

  let updateSpinnedValue = runGUIMVar $ do
        val <- lift $ spinButtonGetValue spinButton
        modifyGUI $ \gui -> gui { testLoader = Just (Split val) }
        runGUIStateWithMVar checkToActivateRunButton
  let updateTestFilePath = runGUIMVar $ do
        path <- lift $ fileChooserGetFilename fileChooserTest
        modifyGUI $ \gui -> gui { testLoader = fmap Path path }
        runGUIStateWithMVar checkToActivateRunButton

  afterValueSpinned spinButton $ do
      updateSpinnedValue
  on fileChooserTest fileChooserButtonFileSet $ do
      updateTestFilePath
  on radioButtonSplit toggled $ do
    active <- toggleButtonGetActive radioButtonSplit
    if active
      then do
        widgetSetSensitive spinButton True
        widgetSetSensitive fileChooserTest False
        updateSpinnedValue
      else return ()
  on radioButtonLoad toggled $ do
    active <- toggleButtonGetActive radioButtonLoad
    if active
      then do
        widgetSetSensitive spinButton False
        widgetSetSensitive fileChooserTest True
        updateTestFilePath
      else return ()
  return ()

-- register check change listener
registerColumnSelectionListeners :: GUIMVar ()
registerColumnSelectionListeners = do
   Just headers <- getsGUI headerWidgets
   let checkbuttons = map (\(_,c,_) -> c) $ Vec.toList headers
   gui <- ask
   let registerListener checkbutton idx =
         on checkbutton toggled $ do
            isActive <- toggleButtonGetActive checkbutton
            runGUIMVar' gui $ onColumnSelectionChanged isActive idx
   lift $ sequence $ zipWith registerListener checkbuttons [0..]
   return ()

onConfigButtonClicked :: GUIState ()
onConfigButtonClicked = do
  -- we can avoid check as button only activates when algo is selected
  Just algorithm <- gets selectedAlgorithm
  Just params <- gets algorithmParams
  params' <- lift $ paramsToConfigWindow params
  modify $ \x -> x { algorithmParams = Just params' }
  return ()

onTableLoadButtonClicked :: CheckButton -> GUIMVar ()
onTableLoadButtonClicked checkButtonHeader = do
    header <- lift $ toggleButtonGetActive checkButtonHeader
    let hasHeader = case header of
          True -> HasHeader
          False -> NoHeader
    -- path must be set when load button is active, so we can skip check
    Just path <- getsGUI trainFilePath
    textTable <- lift $ fromCsv ','  hasHeader path
    case textTable of
        Right tab -> onTableLoad tab
        Left error -> lift $ runErrorDialog error
    return ()

onTableLoad :: RawTable -> GUIMVar ()
onTableLoad textTable = do
  modifyGUI $ \x -> x {
      tableProps = Just (defaultTableProps textTable)
      , loadedTable = Just textTable
    }
  gui <- ask
  lift $ runGUIState' gui $ tableToHBox textTable
  -- register type change listener
  Just headers <- getsGUI headerWidgets
  lift $ for (zip [0..] (Vec.toList headers)) $ \(i, (_,_,combo)) -> on combo changed $ do
      active <- comboBoxGetActive combo
      let update = case active of
            -1 -> []
            0 -> [(i, Numeric)]
            1 -> [(i, Nominal)]
      modifyMVar_ gui $ (>>> return) $ updateTableProps $ \tabProps ->
        tabProps { selectedColumnTypes = Vec.unsafeUpd (selectedColumnTypes tabProps) update }
  Just names <- getsGUI (fmap columnNameStrings . tableProps)
  lift $ runGUIState' gui $ setNewRender (\i -> names Vec.! i)
  updateTargetColumnSelector

onColumnSelectionChanged :: Bool -> Int -> GUIMVar ()
onColumnSelectionChanged isSelected columnIdx = do
  -- there must be columns when user can change selection, so we can avoid check
  Just selected <- getsGUI $ fmap selectedColumns . tableProps
  let selected' = Vec.unsafeUpd selected [(columnIdx, isSelected)]
  modifyGUI $ updateTableProps (\x -> x { selectedColumns = selected' })
  updateTargetColumnSelector
  return ()

updateTargetColumnSelector :: GUIMVar ()
updateTargetColumnSelector = do
  -- there must be a selector initialized when updating it, so we can avoid check
  (combo, model, handler) <- getsGUI targetColSelector
  Just tabProps <- getsGUI tableProps
  let selected = Vec.toList $ selectedColumns tabProps
  let options = Vec.fromList $ map fst $ filter snd $ zip [0..] selected
  gui <- ask
  -- update selector
  handler' <- lift $ do
    listStoreClear model
    traverse (listStoreAppend model) options
    signalDisconnect handler
    on combo changed $ do
      active <- comboBoxGetActive combo
      let option = case active of
            -1 -> Nothing
            i -> Just $ options Vec.! i
      modifyMVar_ gui $ \x -> return $ x { selectedTarget = option }
      withMVar gui $ \x -> (runStateT checkToActivateRunButton) x
      return ()
  modifyGUI $ \x -> x { targetColSelector = (combo, model, handler') }
  getsGUI $ \gui -> (runStateT checkToActivateRunButton) gui
  return ()

removeAllChildren :: ContainerClass c => c -> IO ()
removeAllChildren c = do
  children <- containerGetChildren c
  traverse (containerRemove c) children
  return ()

tableToHBox :: RawTable -> GUIState ()
tableToHBox tab = do
  hbox <- gets tableBox
  cols <- lift $ sequence $ Vec.fromList $ zipWith columnToVBox [0..] $ columns tab
  let (vboxes, headers) = Vec.unzip cols
  modify (\x -> x { headerWidgets = Just headers })
  -- clear and pack hbox containing the table
  lift $ do
    removeAllChildren hbox
    traverse (\col -> boxPackStart hbox col PackNatural 0) $ vboxes
    widgetShowAll hbox
  return ()

columnToVBox :: Int -> RawColumn -> IO (VBox, HeaderWidget)
columnToVBox idx col = do
  let defaultType = either (const Nominal) (const Numeric)
        (parseColumn col :: Fallible NumericColumn)
  -- header
  let name = colName idx col
  (headerBox, checkbutton, combobox) <- mkHeaderWidget name defaultType
  -- column
  let vals = toStrCol col
  treeView <- vecToTreeView vals
  -- default type
  -- assembly
  vbox <- vBoxNew False 0
  boxPackStart vbox headerBox PackNatural 0
  boxPackStart vbox treeView PackNatural 0
  return (vbox, (headerBox, checkbutton, combobox))

toStrCol :: RawColumn -> Vector String
toStrCol (RawColumn (_, vals)) = Vec.map bsToString vals --columnToPrettyPrintables >>> Vec.map defaultPrettyPrint

colName :: Int -> RawColumn -> String
colName idx t =
  (\n -> n ++ " (" ++ show idx ++ ")") $
      either (const "") id $ columnName t

vecToTreeView :: Vector String -> IO TreeView
vecToTreeView vec = do
  model <- listStoreNew (Vec.toList vec)
  treeView <- treeViewNewWithModel model
  -- column
  col <- treeViewColumnNew
  rend <- cellRendererTextNew
  treeViewColumnPackStart col rend True
  cellLayoutSetAttributes col rend model (\row -> [ cellText := row ])
  treeViewColumnSetExpand col True
  treeViewAppendColumn treeView col
  -- disable selection
  selection <- treeViewGetSelection treeView
  treeSelectionSetMode selection SelectionNone
  -- TreeView settings
  treeViewSetHeadersVisible treeView False
  widgetShowAll treeView
  return treeView

removeAllRawColumns view = do
  cols <- treeViewGetColumns view
  traverse (treeViewRemoveColumn view) cols

mkAlgorithmSelector :: MVar GUIGlobalState -> [AlgorithmDescriptor] -> IO ComboBox
mkAlgorithmSelector guiState algoDescs =
  mkComboBox (Vec.fromList algoDescs) show $
    \selected -> modifyMVar_ guiState $ \gui -> do
      let button = configButton gui
      widgetSetSensitive button True
      let gui' = gui {
        selectedAlgorithm = selected,
        algorithmParams = fmap defaultAlgoParams selected
      }
      (runStateT checkToActivateRunButton) gui'
      return gui'

checkToActivateRunButton :: GUIState ()
checkToActivateRunButton = do
  button <- gets runButton
  canActivate <- gets canActivateRunButton
  lift $ widgetSetSensitive button canActivate

{-
  run button should be active iff
    . algorithm is selected
    . target column is selected
    . at least 2 columns are selected
    . test loader is set

  should check whenever selection changes or algorithm changes or test loader changes
  (if we remember target column, than in that case too!)
-}
canActivateRunButton :: GUIGlobalState -> Bool
canActivateRunButton gui =
  algorithmIsSelected && targetColumnIsSelected && numOfSelectedColumns > 1 && testLoaderSet
  where
    algorithmIsSelected = isJust $ selectedAlgorithm gui
    targetColumnIsSelected = isJust $ selectedTarget gui
    numOfSelectedColumns = let count = length . filter id in
      maybe 0 (count . Vec.toList . selectedColumns) $ tableProps gui
    testLoaderSet = isJust $ testLoader gui

mkTargetColumnSelector :: IO (ComboBox, ListStore Int, ConnectId ComboBox)
mkTargetColumnSelector = do
  (combo, model) <- mkComboBox' Vec.empty (const "NO_RENDER")
  widgetSetSizeRequest combo 150 30
  handler <- on combo changed $ return ()
  return (combo, model, handler)


mkComboBox :: Vector a -> (a -> String) -> (Maybe a -> IO ()) -> IO ComboBox
mkComboBox options rend onChange = do
  (combo, _) <- mkComboBox' options rend
  on combo changed $ do
    active <- comboBoxGetActive combo
    let option = case active of
          -1 -> Nothing
          i -> Just $ options Vec.! i
    onChange option
  return combo

setNewRender :: (Int -> String) -> GUIState ()
setNewRender render = do
  (combo, store, handler) <- gets targetColSelector
  lift $ do
    let textColumn = makeColumnIdString 0
    customStoreSetColumn store textColumn render -- set the extraction function
    ren <- cellRendererTextNew
    cellLayoutClear combo
    cellLayoutPackEnd combo ren False
    cellLayoutSetAttributes combo ren store
      (\txt -> [cellText := render txt])

mkComboBox' :: Vector a -> (a -> String) -> IO (ComboBox, ListStore a)
mkComboBox' options render = do
  let textColumn = makeColumnIdString 0
  store <- listStoreNew $ Vec.toList options
  customStoreSetColumn store textColumn render -- set the extraction function
  combo <- comboBoxNewWithModel store
  ren <- cellRendererTextNew
  cellLayoutPackEnd combo ren False
  cellLayoutSetAttributes combo ren store
    (\txt -> [cellText := render txt])
  widgetShow combo
  return (combo, store)

mkHeaderWidget :: ColumnName -> DataType -> IO (VBox, CheckButton, ComboBox)
mkHeaderWidget name defaultType = do
  let allSameSize = False
  let spaceBetween = 20
  box <- vBoxNew allSameSize spaceBetween
  labelName <- labelNew $ Just name
  widgetShow labelName

  checkbutton <- checkButtonNew
  toggleButtonSetActive checkbutton True
  widgetShow checkbutton

  -- combo box
  (combo, _) <- mkComboBox' (Vec.fromList [Numeric, Nominal]) show
  comboBoxSetActive combo $ case defaultType of
    Numeric -> 0
    Nominal -> 1

  boxPackStart box checkbutton PackNatural 0
  boxPackStart box combo PackNatural 0
  boxPackStart box labelName PackNatural 0
  widgetShow box
  return (box, checkbutton, combo)

toStrCols :: DataTable -> [Vector String]
toStrCols t =
  Vec.toList $ printCols
  where
    dataCols = Vec.map dataColumnToPrettyPrintables $ columnsVec t
    printCols = (Vec.map . Vec.map) defaultPrettyPrint dataCols

exampleDT :: DataTable
exampleDT = smartParseTable exampleRawTab

exampleTrain :: RawTable
exampleTrain = exampleNamedFromCols [("a", ["2"]), ("b", ["20"]), ("c", ["200"])]

exampleTest :: RawTable
exampleTest = exampleNamedFromCols [("a", ["1"]), ("b", ["10"]), ("c", ["100"])]

exampleRawTab :: RawTable
exampleRawTab = exampleNamedFromCols [("a", ["1","2"]), ("b", ["10","20"]), ("c", ["100","200"])]

exampleNamedFromCols :: [(String, [String])] -> RawTable
exampleNamedFromCols cols =
  let Right t = tab in t
  where
    tab = do
      namedTable <- for cols $ \(name, vals) -> do
        rawCol <- (fromListToRaw vals)
        nameColumn name rawCol
      fromColumns namedTable

onTrainFileSet :: FileChooserButton -> Button -> GUIState ()
onTrainFileSet fileChooserTrain buttonLoadTrain = do
  (Just f) <- lift $ fileChooserGetFilename fileChooserTrain
  -- change state
  modify $ \x -> x { trainFilePath = Just f }
  -- activate load button
  lift $ widgetSetSensitive buttonLoadTrain True

unsafeSelectColumns :: RawTable -> [Int] -> RawTable
unsafeSelectColumns t = unsafeFromCols . Vec.fromList . map (\i -> cols Vec.! i)
  where
    cols = columnsVec t
