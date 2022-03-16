{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

--import Prelude as P

import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans              (liftIO)

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

import Data.Text.Read
import Data.Either
import Data.Char

-- модель бота
data Model = Model
  { tempTimeUTC :: UTCTime 
  , timeInt :: [Int]
  , tempName :: Text
  , typeAction :: Int
  , listNotes :: [NoteModel]
  , currentTime :: UTCTime
  } deriving (Show)

-- модель содержимого списка напоминаний
data NoteModel = NoteModel
  { noteModelName :: Text
  , noteModelTime :: Maybe UTCTime --время для напоминания
  } deriving (Show)

-- сам бот 
initialBot :: IO (BotApp Model Action)
initialBot = do
  model <- inititalModel
  pure BotApp
    { botInitialModel = model
    , botAction = flip handleCommands
    , botHandler = handler
    , botJobs =
      [ BotJob
        { botJobSchedule = "* * * * *"
        , botJobTask = checkTimeToRemind
        }
      ]
    }

-- первоначальное состояние
inititalModel :: IO Model
inititalModel = do
  now <- myTime
  pure Model { tempTimeUTC = now, timeInt = [], tempName = "", typeAction = 0, listNotes = [], currentTime = now }

-- объявление действий бота
data Action
  = DoNothing
  | SetTime UTCTime
  | IncomingText Text
  | AddTime Text
  | AddMessage
  | DeleteNote Text
  | DeleteAll Text
  | ListItems
  | Start
  | NoteCertainAction Text (Maybe UTCTime)
  | TimeActionsBtns Text (Maybe UTCTime)
  | CreateReminderMin Int Text
  | Menu
  deriving (Show, Read)

myTime :: IO UTCTime
myTime = do
  time <- getCurrentTime
  let ttime = addUTCTime 25200 time
  return ttime

-- добавить текст напоминание в todoitem
addName :: Text -> NoteModel
addName note = NoteModel
  { noteModelName = note
  , noteModelTime = Nothing
  }

-- добавить новый элемент
newNote :: NoteModel -> Model -> Model
newNote item model = model { listNotes = item : listNotes model }

-- удалить из листа
removeItem :: Text -> Model -> Model
removeItem str model = model { listNotes = filter p (listNotes model) }
  where
    p item = noteModelName item /= str

-- удалить все напоминания
removeAll :: Model -> Model
removeAll model = model { listNotes = [] }

-- поставить будильник через отведенное время в мин
createReminderMin :: Int -> Text -> Model -> Model
createReminderMin minutes str model = setupReminder str alarmTime model
  where
    now = currentTime model
    alarmTime = addUTCTime (fromIntegral (60 * minutes)) now

-- установить будильник с определенным временем (ищет напоминание с именем str)
setupReminder :: Text -> UTCTime -> Model -> Model
setupReminder str datetime model = model
  { typeAction = 0, listNotes = map updateReminder (listNotes model) }
    where
      updateReminder item
        | str /= noteModelName item = item
        | otherwise = item { noteModelTime = Just datetime }

-- вывести все напоминалки на кнопках под сообщением
itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard model =
  case listNotes model of
    [] -> "У вас еще нет напоминаний"
    items -> (toEditMessage "Список напоминаний:")
      { editMessageReplyMarkup = Just $
          Telegram.SomeInlineKeyboardMarkup (itemsInlineKeyboard items)
      }

-- клавиатура с каждой кнопкой одного напоминания
itemsInlineKeyboard :: [NoteModel] -> Telegram.InlineKeyboardMarkup
itemsInlineKeyboard
  = Telegram.InlineKeyboardMarkup .  map (pure . itemInlineKeyboardButton)

-- кнопки для выбранного напоминания
itemInlineKeyboardButton :: NoteModel -> Telegram.InlineKeyboardButton
itemInlineKeyboardButton item = actionButton str (NoteCertainAction str time)
  where
    str = noteModelName item
    time = noteModelTime item

-- проверка времени для выдачи напоминания
checkTimeToRemind :: Model -> Eff Action Model
checkTimeToRemind model = do
  newItems <- mapM itemReminder (listNotes model)
  pure model { listNotes = newItems }
  where
    itemReminder item =
      case noteModelTime item of
        Just alarmTime | alarmTime <= currentTime model -> do
          eff $ do
            replyText ("Внимание! Вы просили напомнить о -" <> noteModelName item <> "-")
            return DoNothing
          return item { noteModelTime = Nothing }
        _ -> return item

-- Начальный текст
startMessage :: Text
startMessage = Text.unlines
 [  "Я могу помочь вам создавать и управлять вашими напоминаниями, которые потом я буду",
    "присылать вам через Telegram в указанное вами время.",
    "Также вы можете создавать заметки просто отправляя мне сообщения.\n",
    "Все мои команды: \n/add - создать напоминание \n/list - показать все напоминания",
    "/remove <имя напоминания> - удалить напоминание\n/removeall - удалить все напоминания\n/help - вывести это сообщение"
 ]

timeMessage :: Text
timeMessage = Text.unlines
 [  "Напишите время в формате гг мм дд чч мм.\nНапример 2021 4 1 15 22\n" ]

noteMessage :: Text
noteMessage = Text.unlines
 [ "О чем вам напомнить?\nВведите имя напоминания/заметки:" ]

menuMessage :: Text
menuMessage = Text.unlines
 [ "Нажмите /add, чтобы добавить новое напоминание.\n",
   "Чтобы посмотреть все напоминания, нажмите /list"
 ]

startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/add" , "/list"],
        [ "/removeall" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

noteMessageKeyboard :: Telegram.ReplyKeyboardMarkup
noteMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ ["/exit"] ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

-- действия с напоминанием на кнопках под сообщением при выборе определенного
itemActionsMessage :: Text -> Maybe UTCTime -> EditMessage
itemActionsMessage str time = (toEditMessage ("«" <> str <> "» \nTime: " <> Text.pack (show time)))
  { editMessageReplyMarkup = Just $
      Telegram.SomeInlineKeyboardMarkup (itemActionsKeyboard str time) }

itemActionsKeyboard :: Text -> Maybe UTCTime -> Telegram.InlineKeyboardMarkup
itemActionsKeyboard str time = Telegram.InlineKeyboardMarkup
  [ [ btnRemove ] , [ btnAddTime ] , [ btnBack ] ]
  where
      btnRemove = actionButton "Выполнено" (DeleteNote str)
      btnBack   = actionButton "Вернуться к списку" ListItems
      btnAddTime = actionButton "Установить время" (TimeActionsBtns str time)

timeActionsMessage :: Text -> Maybe UTCTime -> EditMessage
timeActionsMessage str time = (toEditMessage ("«" <> str <> "» \nВыберите время: " ))
  { editMessageReplyMarkup = Just $
      Telegram.SomeInlineKeyboardMarkup (timeActionsKeyboard str time) }

timeActionsKeyboard :: Text -> Maybe UTCTime -> Telegram.InlineKeyboardMarkup
timeActionsKeyboard str time = Telegram.InlineKeyboardMarkup
  [ [ buttonReminder 1, buttonReminder 20, buttonReminder 60 ]
  , [ buttonReminderH 1, buttonReminderH 5, buttonReminderH 24 ]
  , [ btnAddTime ]
  , [ btnBack ]
  ] 
  where 
      buttonReminder n = actionButton
        (Text.pack (show n) <> " мин")
        (CreateReminderMin n str)
      buttonReminderH n = actionButton
        (Text.pack (show n) <> " час")
        (CreateReminderMin (n*60) str)
      btnAddTime = actionButton "Дополнительно..." (AddTime str)
      btnBack   = actionButton "Вернуться" (NoteCertainAction str time)


-- входящие от Tg.Update закидываем в Action's.
handleCommands :: Model -> Telegram.Update -> Maybe Action
handleCommands _ = parseUpdate
    $ ListItems   <$  command "list" 
  <|> DeleteNote <$> command "remove"
  <|> DeleteAll <$> command "removeall" 
  <|> Start <$  (command "start" <|> command "help")
  <|> callbackQueryDataRead 
  <|> AddMessage <$  (command "add")
  <|> Menu <$ (command "exit")
  <|> IncomingText     <$> text

exSeconds :: Integer -> Integer -> Integer
exSeconds hours minutes
  | hours == 0 = minutes * 60
  | minutes == 0 = hours * 3600
  | otherwise = hours * 3600 + minutes * 60

addTextTime :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
addTextTime year month day hours minutes = UTCTime (fromGregorian year month day) (secondsToDiffTime ( exSeconds hours minutes ) + 59 )

checkDate :: Int -> Int -> Int -> Bool
checkDate d m y = d>0 && m>0 && m<13 && y>2020 && d <=
    (if m `elem` [4,6,9,11] then 30
     else 
        if m == 2 then
            if (y `mod` 400 == 0) || (y `mod` 100 /= 0) && 
                (y `mod` 4 == 0) then 29
            else 28
        else 31)

readNums :: Text -> [Int]
readNums =
  map fst .
  snd .  partitionEithers .
  map decimal .
  filter (not . Text.null) .
  Text.split (not . isDigit)

textTime :: Text -> Text -> Model -> Model
textTime note time model
  | checkDate (timeArr !! 2) (timeArr !! 1) (timeArr !! 0) = setupReminder note (addTextTime (toInteger(timeArr !! 0)) (timeArr !! 1) (timeArr !! 2) (toInteger(timeArr !! 3)) (toInteger(timeArr !! 4) - 1)) model
  | otherwise = model { typeAction = 0 }
    where timeArr = readNums time

addItemCase :: Text -> Model -> Model
addItemCase str model 
  | action == 0 = newNote (addName str) model
  | otherwise = textTime note str model
    where action = typeAction model
          note = tempName model

-- описание действий
handler :: Action -> Model -> Eff Action Model
handler action model = case action of
  DoNothing -> pure model
  SetTime t -> model { currentTime = t } <# do
    SetTime <$> liftIO (threadDelay 1000 >> myTime)
  AddMessage -> do
    eff $ do
      reply (toReplyMessage noteMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup noteMessageKeyboard
        }
      pure DoNothing
    pure model
  IncomingText str -> addItemCase str model <# do
    if ((typeAction model) == 0) then replyText ("Заметка добавлена. Чтобы открыть список заметок введите /list \nили введите еще одну заметку")
    else replyText ("Время добавлено. Чтобы открыть список напоминаний введите /list")
    pure DoNothing
  DeleteNote str -> removeItem str model <# do
    replyText ("Удалено: " <> str)
    pure ListItems
  DeleteAll str -> removeAll model <# do
    replyText ("Удалены все напоминания." <> str)
    pure DoNothing
  ListItems -> model <# do
    replyOrEdit (itemsAsInlineKeyboard model)
    pure DoNothing
  Start -> do
    eff $ do
      reply (toReplyMessage startMessage)
      reply (toReplyMessage menuMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
        }
      pure DoNothing
    eff $ SetTime <$> liftIO myTime
    pure model
  Menu -> do
    eff $ do
      reply (toReplyMessage menuMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
        }
      pure DoNothing
    pure model { typeAction = 0 }
  AddTime str -> do
    eff $ do 
      reply (toReplyMessage timeMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup noteMessageKeyboard
        }
      pure DoNothing
    pure model { tempName = str, typeAction = 1 }
  NoteCertainAction str time -> model <# do
    editUpdateMessage (itemActionsMessage str time)
    pure DoNothing
  TimeActionsBtns str time -> model <# do
    editUpdateMessage (timeActionsMessage str time)
    pure DoNothing
  CreateReminderMin minutes str -> createReminderMin minutes str model <# do
    replyText ("Хорошо, я напомню вам о -" <> str <> "- через " <> (Text.pack (show minutes)) <> " минут.")
    pure DoNothing

main :: IO ()
main = do
  env <- Telegram.defaultTelegramClientEnv "1779930678:AAHJaHhclY0zrKcBb9vkSaPk2YgCPltqrH8"
  moksyaBot <- initialBot
  startBot_ (useLatestUpdateInJobs (traceBotDefault moksyaBot)) env
