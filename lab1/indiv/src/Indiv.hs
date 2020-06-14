module Indiv
    ( menu,
    dateStrFromSql, strToDate, dateToSql,
    strToDateTime, dateTimeToSql, dateTimeStrFromSql,
    boolToSql,
    exec_insert, exec_delete,
    insert_rec, update_rec, delete_rec,
    Group(..), Student(..), Lecturer(..), Topic(..), 
    Task(..), Question(..), Material(..),
    find_group, find_student, find_student_gr, find_lecturer,
    find_topic, find_task, find_question, find_material,
    ) where

import Database.HDBC
import Data.Time
import Data.Time.Format
import Debug.Trace

-- ввод целого числа
getInt :: IO Int
getInt = do  
  s <- getLine
  return (read s)
  
-- ввод даты
getDate :: IO UTCTime
getDate = do  
  s <- getLine
  return $ strToDate s

-- ввод даты и времени
getDateTime :: IO UTCTime
getDateTime = do  
  s <- getLine
  return $ strToDateTime s
  
-- ввод логического значения
getBool :: IO Bool
getBool = do  
  s <- getLine
  return $ case s of
    "д" -> True
    "да" -> True
    "y" -> True
    "yes" -> True
    _ -> False

-- получение даты из строки
strToDate :: String -> UTCTime
strToDate str = parseTimeOrError True defaultTimeLocale "%d.%m.%Y" str

-- получение даты и времени из строки
strToDateTime :: String -> UTCTime
strToDateTime str = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M" str

-- перевод даты в строку для отображения
dateToStr :: UTCTime -> String
dateToStr date = 
  formatTime defaultTimeLocale "%d.%m.%Y" date

-- перевод даты и времени в строку для отображения
dateTimeToStr :: UTCTime -> String
dateTimeToStr time = 
  formatTime defaultTimeLocale "%d.%m.%Y %H:%M" time
  
-- перевод даты в строку для sql
dateToSql :: UTCTime -> SqlValue
dateToSql date = 
  toSql $ formatTime defaultTimeLocale "%Y-%m-%d" date

-- перевод даты и времени в строку для sql
dateTimeToSql :: UTCTime -> SqlValue
dateTimeToSql time = 
  toSql $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
   
-- разбор даты из результата запроса SQL
dateFromSql :: SqlValue -> UTCTime
dateFromSql value =
  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (fromSql value)

-- разбор даты и времени из результата запроса SQL
dateTimeFromSql :: SqlValue -> UTCTime
dateTimeFromSql value =
  parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (fromSql value)

-- перевод даты из результата запроса SQL в строку для отображения
dateStrFromSql :: SqlValue -> String
dateStrFromSql value =
  dateToStr $ dateFromSql value

-- перевод даты и времени из результата запроса SQL в строку для отображения
dateTimeStrFromSql :: SqlValue -> String
dateTimeStrFromSql value =
  dateTimeToStr $ dateTimeFromSql value

-- преобразование логического значения в целое значение для SQL
boolToSql :: Bool -> SqlValue
boolToSql True = toSql (1 ::Integer)
boolToSql False = toSql (0 ::Integer)

-- выполнение запроса с обработкой исключений
exec_sql :: IConnection conn => conn -> String -> [SqlValue] -> String -> String -> IO (Bool)
exec_sql conn sql params mes_ok mes_err = do
  r <- handleSql 
        (\e@(SqlError _ _ mes) -> do traceM mes ; return 0)
        (run conn sql params)
  if (r > 0)
    then do commit conn ; putStrLn mes_ok
    else do rollback conn ; putStrLn mes_err
  return (r > 0)  
  
exec_insert :: IConnection conn => conn -> String -> [SqlValue] -> IO (Bool)
exec_insert conn sql params =
  exec_sql conn sql params 
    "Запись добавлена успешно"
    "Ошибка при добавлении записи"
    
exec_update :: IConnection conn => conn -> String -> [SqlValue] -> IO (Bool)
exec_update conn sql params =
  exec_sql conn sql params 
    "Запись обновлена успешно"
    "Ошибка при обновлении записи"    

exec_delete :: IConnection conn => conn -> String -> [SqlValue] -> IO (Bool)
exec_delete conn sql params =
  exec_sql conn sql params 
    "Запись удалена успешно"
    "Ошибка при удалении записи"  

    
class Record a where
  rec_id :: a -> Int
  
  table :: a -> String
  
  insert_rec :: IConnection conn => conn -> a -> IO (Bool)
  update_rec :: IConnection conn => conn -> a -> IO (Bool)

  -- удаление записи
  delete_rec :: IConnection conn => conn -> a -> IO (Bool)
  delete_rec conn rec =
    exec_delete conn ("DELETE FROM " ++ (table rec) ++ " WHERE id=?") [toSql (rec_id rec)]
    
----------------------------------------------------------------------------    

-- структура Группа
data Group = Group {
          grId :: Int,
          grNumb :: String 
          } deriving( Eq )

instance Record Group where

  -- код записи
  rec_id Group{ grId = id } = id

  -- имя таблицы в БД
  table Group{} = "sgroup"

  -- добавление записи
  insert_rec conn (Group{ grNumb = numb }) =
    exec_insert conn "INSERT INTO sgroup(numb) VALUES (?)" [toSql numb]
    
  -- обновление записи
  update_rec conn (Group{ grId = id, grNumb = numb }) =
    exec_update conn "UPDATE sgroup SET numb=? WHERE id=?" [toSql numb, toSql id]
    
    
-- преобразование структуры в строку для отображения    
instance Show Group where
  show (Group{ grId = id, grNumb = numb }) = "код " ++ show id ++ ", номер " ++ numb
  
-- Ввод информации о группе
get_group :: Int -> IO Group
get_group id = do
  putStrLn "Номер группы: "
  numb <- getLine
  return Group {
            grId = id, 
            grNumb = numb
            }
            
-- создание структуры Группа из списка sql-значений
new_group :: [SqlValue] -> Maybe Group
new_group [id, numb] = Just $ Group { grId = fromSql id, grNumb = fromSql numb }
new_group _ = Nothing

-- поиск группы по id
find_group :: IConnection conn => conn -> Int -> IO (Maybe Group)
find_group conn id = do
  r <- quickQuery' conn "SELECT id, numb FROM sgroup WHERE id=?" [toSql id]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_group row
    
----------------------------------------------------------------------------    

-- структура Студент
data Student = Student { 
         stId :: Int,
         stName :: String,
         stSurname :: String,
         stMiddlename :: String,
         stBirthdate :: UTCTime,
         stGroupId :: Int
         } deriving( Eq )

instance Record Student where

  -- код записи
  rec_id Student{ stId = id } = id

  -- имя таблицы в БД
  table Student{} = "student"

  -- добавление записи
  insert_rec conn Student{ stName = name, stSurname = surname,
                stMiddlename = middlename, stBirthdate = birthdate,
                stGroupId = group_id } = 
    exec_insert conn ("INSERT INTO student(name,surname,middlename,birthdate,sgroup_id)"
                     ++ " VALUES (?,?,?,?,?)")
                [toSql name, toSql surname, toSql middlename, dateToSql birthdate,
                 toSql group_id]
    
  -- обновление записи
  update_rec conn Student{ stId = id, stName = name, stSurname = surname,
                stMiddlename = middlename, stBirthdate = birthdate,
                stGroupId = group_id } =
    exec_update conn ("UPDATE student SET name=?,surname=?,middlename=?,birthdate=?,sgroup_id=?"
                ++ " WHERE id=?") 
                [toSql name, toSql surname, toSql middlename, dateToSql birthdate,
                 toSql group_id, toSql id]
    
    
-- преобразование структуры в строку для отображения    
instance Show Student where
  show Student{ stId = id, stName = name, stSurname = surname,
              stMiddlename = middlename, stBirthdate = birthdate,
              stGroupId = group_id } =
    "код " ++ show id 
      ++ ", " ++ name ++ " " ++ surname ++ " " ++ middlename 
      ++ ", дата рожд. " ++ (dateToStr birthdate)

           
-- Ввод информации о студенте
get_student :: Int -> Group -> IO Student
get_student id group = do
  putStrLn "Фамилия: "
  name <- getLine
  putStrLn "Имя: "
  surname <- getLine
  putStrLn "Отчество: "
  middlename <- getLine
  putStrLn "Дата рождения: "
  birthdate <- getDate
  return Student{
            stId = id, 
            stName = name, 
            stSurname = surname,
            stMiddlename = middlename, 
            stBirthdate = birthdate,
            stGroupId = (rec_id group) 
            }
            
-- создание структуры Студент из списка sql-значений
new_student :: [SqlValue] -> Maybe Student
new_student [id, name, surname, middlename, birthdate, group_id] = 
   Just $ Student { stId = fromSql id, stName = fromSql name, 
               stSurname = fromSql surname, stMiddlename = fromSql middlename, 
               stBirthdate = dateFromSql birthdate,
               stGroupId = fromSql group_id }
new_student _ = Nothing
            
-- поиск студента по id
find_student :: IConnection conn => conn -> Int -> IO (Maybe Student)
find_student conn id = do
  r <- quickQuery' conn ("SELECT id,name,surname,middlename,birthdate,sgroup_id"
                     ++ " FROM student WHERE id=?") 
                     [toSql id]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_student row

-- поиск студента по id и группе
find_student_gr :: IConnection conn => conn -> Int -> Group -> IO (Maybe Student)
find_student_gr conn id group = do
  r <- quickQuery' conn ("SELECT id,name,surname,middlename,birthdate,sgroup_id"
                     ++ " FROM student WHERE id=? AND sgroup_id=?") 
                     [toSql id, toSql (rec_id group)]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_student row

----------------------------------------------------------------------------    

-- структура Преподаватель
data Lecturer = Lecturer { 
         lcId :: Int,
         lcName :: String,
         lcSurname :: String,
         lcMiddlename :: String,
         lcBirthdate :: UTCTime
         } deriving( Eq )

instance Record Lecturer where

  -- код записи
  rec_id Lecturer{ lcId = id } = id

  -- имя таблицы в БД
  table Lecturer{} = "lecturer"

  -- добавление записи
  insert_rec conn (Lecturer{ lcName = name, lcSurname = surname,
                lcMiddlename = middlename, lcBirthdate = birthdate }) = 
    exec_insert conn "INSERT INTO lecturer(name,surname,middlename,birthdate) VALUES (?,?,?,?)"
                [toSql name, toSql surname, toSql middlename, dateToSql birthdate]
    
  -- обновление записи
  update_rec conn Lecturer{ lcId = id, lcName = name, lcSurname = surname,
                lcMiddlename = middlename, lcBirthdate = birthdate} =
    exec_update conn ("UPDATE lecturer SET name=?,surname=?,middlename=?,birthdate=?"
                ++ " WHERE id=?") 
                [toSql name, toSql surname, toSql middlename, dateToSql birthdate, toSql id]
    
    
-- преобразование структуры в строку для отображения  
instance Show Lecturer where
  show (Lecturer{ lcId = id, lcName = name, lcSurname = surname,
              lcMiddlename = middlename, lcBirthdate = birthdate }) =
    "код " ++ show id 
      ++ ", " ++ name ++ " " ++ surname ++ " " ++ middlename 
      ++ ", дата рожд. " ++ (dateToStr birthdate)

           
-- ввод информации о преподавателе  
get_lecturer :: Int -> IO Lecturer
get_lecturer id = do
  putStrLn "Фамилия: "
  name <- getLine
  putStrLn "Имя: "
  surname <- getLine
  putStrLn "Отчество: "
  middlename <- getLine
  putStrLn "Дата рождения: "
  birthdate <- getDate
  return Lecturer{
            lcId = id, 
            lcName = name, 
            lcSurname = surname,
            lcMiddlename = middlename, 
            lcBirthdate = birthdate 
            }
            
-- создание структуры Преподаватель из списка sql-значений
new_lecturer :: [SqlValue] -> Maybe Lecturer
new_lecturer [id, name, surname, middlename, birthdate] = 
   Just $ Lecturer { lcId = fromSql id, lcName = fromSql name, 
               lcSurname = fromSql surname, lcMiddlename = fromSql middlename, 
               lcBirthdate = dateFromSql birthdate }
new_lecturer _ = Nothing
            
-- поиск преподавателя по id
find_lecturer :: IConnection conn => conn -> Int -> IO (Maybe Lecturer)
find_lecturer conn id = do
  r <- quickQuery' conn ("SELECT id,name,surname,middlename,birthdate"
                     ++ " FROM lecturer WHERE id=?") [toSql id]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_lecturer row

----------------------------------------------------------------------------    

-- структура Тема
data Topic = Topic {
         tpId :: Int,             -- код
         tpName :: String,        -- название темы
         tpTime :: UTCTime,       -- время начала
         tpLecturerId :: Int     -- код преподавателя
         } deriving( Eq )

instance Record Topic where

  -- код записи
  rec_id Topic{ tpId = id } = id

  -- имя таблицы в БД
  table Topic{} = "topic"

  -- добавление записи
  insert_rec conn (Topic{ tpName = name, tpTime = time, tpLecturerId = lecturer_id }) = 
    exec_insert conn "INSERT INTO topic(name,stime,lecturer_id) VALUES (?,?,?)"
                [toSql name, dateTimeToSql time, toSql lecturer_id]
    
  -- обновление записи
  update_rec conn Topic{ tpId = id, tpName = name, tpTime = time, tpLecturerId = lecturer_id} =
    exec_update conn ("UPDATE topic SET name=?,stime=?,lecturer_id=? WHERE id=?")
                [toSql name, dateTimeToSql time, toSql lecturer_id, toSql id]
    
    
-- преобразование структуры в строку для отображения    
instance Show Topic where
  show (Topic{ tpId = id, tpName = name, tpTime = time}) =
    "код " ++ show id 
      ++ ", " ++ name
      ++ ", время " ++ (dateTimeToStr time)

           
-- ввод информации о теме  
get_topic :: IConnection conn => conn -> Int -> IO (Maybe Topic)
get_topic conn id = do
  putStrLn "Тема: "
  name <- getLine
  
  putStrLn "Дата и время занятия (ДД.ММ.ГГГГ ЧЧ:ММ): "
  time <- getDateTime
  
  putStrLn "Код преподавателя: "
  lecturer_id <- getInt
  r_lecturer <- find_lecturer conn lecturer_id
  case r_lecturer of
  
    Nothing -> do
      putStrLn "Преподаватель не найден"
      return Nothing
      
    Just lecturer -> do
      putStrLn $ "Преподаватель: " ++ show lecturer
      return $ Just Topic{
            tpId = id, 
            tpName = name, 
            tpTime = time,
            tpLecturerId = lecturer_id
            }     
            
-- создание структуры Преподаватель из списка sql-значений
new_topic :: [SqlValue] -> Maybe Topic
new_topic [id, name, time, lecturer_id] = 
   Just $ Topic { tpId = fromSql id, tpName = fromSql name, 
      tpTime = dateTimeFromSql time, tpLecturerId = fromSql lecturer_id }
new_topic _ = Nothing
            
-- поиск преподавателя по id
find_topic :: IConnection conn => conn -> Int -> IO (Maybe Topic)
find_topic conn id = do
  r <- quickQuery' conn ("SELECT id,name,stime,lecturer_id"
                     ++ " FROM topic WHERE id=?") [toSql id]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_topic row

----------------------------------------------------------------------------    

-- структура Задача
data Task = Task { 
         tkId :: Int,
         tkName :: String,
         tkComplete :: Bool,
         tkTopicId :: Int
         } deriving( Eq )

instance Record Task where

  -- код записи
  rec_id Task{ tkId = id } = id

  -- имя таблицы в БД
  table Task{} = "task"

  -- добавление записи
  insert_rec conn Task{ tkName = name, tkComplete = complete, tkTopicId = topic_id } = 
    exec_insert conn ("INSERT INTO task(name,complete,topic_id)"
                     ++ " VALUES (?,?,?)")
                [toSql name, boolToSql complete, toSql topic_id]
    
  -- обновление записи
  update_rec conn Task{ tkId = id, tkName = name, tkComplete = complete, tkTopicId = topic_id } =
    exec_update conn ("UPDATE task SET name=?,complete=?,topic_id=?"
                ++ " WHERE id=?") 
                [toSql name, boolToSql complete, toSql topic_id, toSql id]
    
-- преобразование структуры в строку для отображения    
instance Show Task where
  show Task{ tkId = id, tkName = name, tkComplete = complete } =
    "код " ++ show id 
      ++ ", " ++ name
      ++ ", " ++ if complete then "выполнена" else "не выполнена"

           
-- Ввод информации о задаче
get_task :: Int -> Topic -> IO Task
get_task id topic = do
  putStrLn "Задача: "
  name <- getLine
  putStrLn "Завершена (д/н): "
  complete <- getBool
  return Task{
            tkId = id, 
            tkName = name, 
            tkComplete = complete,
            tkTopicId = (rec_id topic) 
            }
            
-- создание структуры Задача из списка sql-значений
new_task :: [SqlValue] -> Maybe Task
new_task [id, name, complete, topic_id] = 
   Just $ Task { tkId = fromSql id, tkName = fromSql name, 
               tkComplete = fromSql complete, tkTopicId = fromSql topic_id }
new_task _ = Nothing
            
-- поиск задачи по id
find_task :: IConnection conn => conn -> Int -> Topic -> IO (Maybe Task)
find_task conn id topic = do
  r <- quickQuery' conn ("SELECT id,name,complete,topic_id"
                     ++ " FROM task WHERE id=? AND topic_id=?")
                     [toSql id, toSql (rec_id topic)]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_task row

----------------------------------------------------------------------------    

-- структура Вопрос
data Question = Question { 
         quId :: Int,            -- код вопроса
         quQuestion :: String,   -- вопрос студента
         quAnswer :: String,     -- ответ преподавателя
         quTopicId :: Int,       -- код темы
         quStudentId :: Int      -- код студента
         } deriving( Eq )

instance Record Question where

  -- код записи
  rec_id Question{ quId = id } = id

  -- имя таблицы в БД
  table Question{} = "question"

  -- добавление записи
  insert_rec conn Question{ quQuestion = question, quAnswer = answer, 
                             quTopicId = topic_id, quStudentId = student_id} = 
    exec_insert conn ("INSERT INTO question(question,answer,topic_id,student_id)"
                     ++ " VALUES (?,?,?,?)")
                [toSql question, toSql answer, toSql topic_id, toSql student_id]
    
  -- обновление записи
  update_rec conn Question{ quId = id, quQuestion = question, quAnswer = answer, 
                             quTopicId = topic_id, quStudentId = student_id } =
    exec_update conn ("UPDATE question SET question=?,answer=?,topic_id=?,student_id=?"
                     ++ " WHERE id=?") 
                [toSql question, toSql answer, toSql topic_id, toSql student_id, toSql id]
    
-- преобразование структуры в строку для отображения
instance Show Question where
  show Question{ quId = id, quQuestion = question, quAnswer = answer } =
    "код " ++ show id 
      ++ ", вопрос: " ++ question
      ++ ", ответ:" ++ answer
           
-- Ввод информации о задаче
get_question :: IConnection conn => conn -> Int -> Topic -> IO (Maybe Question)
get_question conn id topic = do
  putStrLn "Вопрос: "
  question <- getLine

  putStrLn "Ответ: "
  answer <- getLine
  
  putStrLn "Код студента: "
  student_id <- getInt
  r_student <- find_student conn student_id
  case r_student of
  
    Nothing -> do
      putStrLn "Студент не найден"
      return Nothing
      
    Just student -> do
      putStrLn $ "Студент: " ++ show student
      return $ Just Question{
            quId = id, 
            quQuestion = question, 
            quAnswer = answer,
            quTopicId = (rec_id topic),
            quStudentId = student_id
            }     
            
-- создание структуры Задача из списка sql-значений
new_question :: [SqlValue] -> Maybe Question
new_question [id, question, answer, topic_id, student_id] = 
   Just $ Question { quId = fromSql id, quQuestion = fromSql question, 
               quAnswer = fromSql answer, quTopicId = fromSql topic_id,
               quStudentId = fromSql student_id }
new_question _ = Nothing
            
-- поиск задачи по id
find_question :: IConnection conn => conn -> Int -> Topic -> IO (Maybe Question)
find_question conn id topic = do
  r <- quickQuery' conn ("SELECT id,question,answer,topic_id,student_id"
                     ++ " FROM question WHERE id=? AND topic_id=?")
                     [toSql id, toSql (rec_id topic)]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_question row

----------------------------------------------------------------------------    

-- структура Дополнительный материал
data Material = Material { 
         mtId :: Int,         -- код материала
         mtName :: String,    -- название материала   
         mtTopicId :: Int     -- код темы
         } deriving( Eq )

instance Record Material where

  -- код записи
  rec_id Material{ mtId = id } = id

  -- имя таблицы в БД
  table Material{} = "material"

  -- добавление записи
  insert_rec conn Material{ mtName = name, mtTopicId = topic_id } = 
    exec_insert conn ("INSERT INTO material(name,topic_id)"
                     ++ " VALUES (?,?)")
                [toSql name, toSql topic_id]
    
  -- обновление записи
  update_rec conn Material{ mtId = id, mtName = name, mtTopicId = topic_id } =
    exec_update conn ("UPDATE material SET name=?,topic_id=? WHERE id=?") 
                [toSql name, toSql topic_id, toSql id]
    
-- преобразование структуры в строку для отображения    
instance Show Material where
  show Material{ mtId = id, mtName = name } =
    "код " ++ show id 
      ++ ", " ++ name
           
-- Ввод информации о материале
get_material :: Int -> Topic -> IO Material
get_material id topic = do
  putStrLn "Название материала: "
  name <- getLine
  return Material{
            mtId = id, 
            mtName = name, 
            mtTopicId = (rec_id topic) 
            }
            
-- создание структуры Дополнительный материал из списка sql-значений
new_material :: [SqlValue] -> Maybe Material
new_material [id, name, topic_id] = 
   Just $ Material { mtId = fromSql id, mtName = fromSql name, 
                        mtTopicId = fromSql topic_id }
new_material _ = Nothing
            
-- поиск задачи по id
find_material :: IConnection conn => conn -> Int -> Topic -> IO (Maybe Material)
find_material conn id topic = do
  r <- quickQuery' conn ("SELECT id,name,topic_id"
                     ++ " FROM material WHERE id=? AND topic_id=?")
                     [toSql id, toSql (rec_id topic)]
  return $ case r of
    [] -> Nothing
    (row:_) -> new_material row

----------------------------------------------------------------------------    

-- вывод список пунктов меню
print_list :: [(Int, String)] -> IO ()
print_list [] = return ()
print_list ((id, text) : mlist) = do
  putStrLn $ show id ++ "." ++ text
  print_list mlist
  
-- показать список задач
show_tasks :: IConnection conn => conn -> Topic -> IO ()
show_tasks conn topic = do
  putStrLn $ "Тема " ++ tpName topic ++ ":"
  putStrLn "Код | Задача | Выполнена"
  r <- quickQuery' conn ("SELECT id,name,complete"
        ++ " FROM task WHERE topic_id=?")
        [toSql (rec_id topic)]
  mapM_ (\x -> do
           let [id,name,complete] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ (fromSql name) ++ " " 
                ++ " | "  ++ (if (fromSql complete) :: Bool then "Да" else "Нет")
           return ()
         ) r

-- добавление задачи
task_add :: IConnection conn => conn -> Topic -> IO ()
task_add conn topic = do
  putStrLn "\nДобавление задачи"
  task <- get_task 0 topic
  insert_rec conn task
  return ()
  
-- изменение задачи
task_edit :: IConnection conn => conn -> Topic -> IO ()
task_edit conn topic = do
  putStrLn "\nИзменение задачи"
  putStrLn "Код задачи: "
  id <- getInt
  r_task <- find_task conn id topic
  case r_task of
    Just task  -> do
      putStrLn $ "Изменяемая задача: " ++ show task
      task <- get_task id topic
      update_rec conn task
      return ()
    Nothing ->
      putStrLn "Задача не найден"
  
-- удаление задачи
task_del :: IConnection conn => conn -> Topic -> IO ()
task_del conn topic = do
  putStrLn "Удаление задачи"
  putStrLn "Код задачи: "
  id <- getInt
  r_task <- find_task conn id topic
  case r_task of
    Just task  -> do
      putStrLn $ "Задача: " ++ show task
      delete_rec conn task
      return ()
    Nothing ->
      putStrLn "Задача не найдена"

-- меню Задачи
menu_tasks :: IConnection conn => conn -> Topic -> IO ()
menu_tasks conn topic = do
  putStrLn $ "\n[Задачи темы " ++ tpName topic  ++ "]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_tasks conn topic
        menu_tasks conn topic
    "2" -> do
        task_add conn topic
        menu_tasks conn topic
    "3" -> do
        task_edit conn topic
        menu_tasks conn topic
    "4" -> do
        task_del conn topic
        menu_tasks conn topic
    "0" ->
         return ()
    otherwise ->
        menu_tasks conn topic

-- показать список вопросов
show_questions :: IConnection conn => conn -> Topic -> IO ()
show_questions conn topic = do
  putStrLn $ "Тема " ++ tpName topic ++ ":"
  putStrLn "Код | Вопрос | Ответ | Студент"
  r <- quickQuery' conn ("SELECT q.id,q.question,q.answer,s.name as student_name"
        ++ " FROM question as q LEFT JOIN student as s ON q.student_id=s.id"
        ++ " WHERE topic_id=?")
        [toSql (rec_id topic)]
  mapM_ (\x -> do
           let [id,question,answer,student_name] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ fromSql question ++ " " 
                ++ " | "  ++ fromSql answer ++ " " 
                ++ " | "  ++ fromSql student_name
           return ()
         ) r

-- добавление вопроса
question_add :: IConnection conn => conn -> Topic -> IO ()
question_add conn topic = do
  putStrLn "\nДобавление вопроса"
  m_question <- get_question conn 0 topic
  case m_question of
    Nothing -> return ()
    Just question -> do 
      insert_rec conn question
      return ()
  
-- изменение вопроса
question_edit :: IConnection conn => conn -> Topic -> IO ()
question_edit conn topic = do
  putStrLn "\nИзменение вопроса"
  putStrLn "Код вопроса: "
  id <- getInt
  r_question <- find_question conn id topic
  case r_question of
    Just question  -> do
      putStrLn $ "Изменяемый вопрос: " ++ show question
      insert_rec conn question
      return ()
    Nothing ->
      putStrLn "Вопрос не найден"
  
-- удаление вопроса
question_del :: IConnection conn => conn -> Topic -> IO ()
question_del conn topic = do
  putStrLn "Удаление вопроса"
  putStrLn "Код вопроса: "
  id <- getInt
  r_question <- find_question conn id topic
  case r_question of
    Just question  -> do
      putStrLn $ "Вопрос: " ++ show question
      delete_rec conn question
      return ()
    Nothing ->
      putStrLn "Вопрос не найдена"

-- меню Вопросы
menu_questions :: IConnection conn => conn -> Topic -> IO ()
menu_questions conn topic = do
  putStrLn $ "\n[Вопросы  темы " ++ tpName topic  ++ "]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_questions conn topic
        menu_questions conn topic
    "2" -> do
        question_add conn topic
        menu_questions conn topic
    "3" -> do
        question_edit conn topic
        menu_questions conn topic
    "4" -> do
        question_del conn topic
        menu_questions conn topic
    "0" ->
         return ()
    otherwise ->
        menu_questions conn topic

-- показать список материалов
show_materials conn topic = do
  putStrLn $ "Дополнительные материалы по теме " ++ tpName topic ++ ":"
  putStrLn "Код | Название | Выполнена"
  r <- quickQuery' conn ("SELECT id,name"
        ++ " FROM material WHERE topic_id=?")
        [toSql (rec_id topic)]
  mapM_ (\x -> do
           let [id,name,complete] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ (fromSql name) ++ " " 
                ++ " | "  ++ (if (fromSql complete) :: Bool then "Да" else "Нет")
           return ()
         ) r

-- добавление материала
material_add :: IConnection conn => conn -> Topic -> IO ()
material_add conn topic = do
  putStrLn "\nДобавление материала"
  material <- get_material 0 topic
  insert_rec conn material
  return ()
  
-- изменение материала
material_edit :: IConnection conn => conn -> Topic -> IO ()
material_edit conn topic = do
  putStrLn "\nИзменение материала"
  putStrLn "Код материала: "
  id <- getInt
  r_material <- find_material conn id topic
  case r_material of
    Just material  -> do
      putStrLn $ "Изменяемый материал: " ++ show material
      material <- get_material id topic
      update_rec conn material
      return ()
    Nothing ->
      putStrLn "Материал не найден"
  
-- удаление материала
material_del :: IConnection conn => conn -> Topic -> IO ()
material_del conn topic = do
  putStrLn "Удаление материала"
  putStrLn "Код материала: "
  id <- getInt
  r_material <- find_material conn id topic
  case r_material of
    Just material  -> do
      putStrLn $ "Материал: " ++ show material
      delete_rec conn material
      return ()
    Nothing ->
      putStrLn "Материал не найдена"

-- меню Дополнительные материалы
menu_materials :: IConnection conn => conn -> Topic -> IO ()
menu_materials conn topic = do
  putStrLn $ "\n[Дополнительные материалы по теме: " ++ tpName topic  ++ "]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_materials conn topic
        menu_materials conn topic
    "2" -> do
        material_add conn topic
        menu_materials conn topic
    "3" -> do
        material_edit conn topic
        menu_materials conn topic
    "4" -> do
        material_del conn topic
        menu_materials conn topic
    "0" ->
         return ()
    otherwise ->
        menu_materials conn topic


-- меню Тема
menu_topic :: IConnection conn => conn -> Topic -> IO ()
menu_topic conn topic = do
  putStrLn $ "\n[Тема " ++ (tpName topic) ++ "]"
  print_list [
       (1, "Задачи"),
       (2, "Вопросы"),
       (3, "Дополнительные материалы"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        menu_tasks conn topic
        menu_topic conn topic
    "2" -> do
        menu_questions conn topic
        menu_topic conn topic
    "3" -> do
        menu_materials conn topic
        menu_topic conn topic
    "0" ->
        return ()
    otherwise -> 
        menu_topic conn topic
  
-- выбор темы для меню Тема
topic_sel :: IConnection conn => conn -> IO ()
topic_sel conn = do
  putStrLn "Код темы: "
  id <- getInt
  r_topic <- find_topic conn id
  case r_topic of
    Just topic  -> do
      menu_topic conn topic
    Nothing ->
      putStrLn "Тема не найдена"  

-- показать список тем
show_topics :: IConnection conn => conn -> IO ()
show_topics conn = do
  putStrLn "Темы:"
  putStrLn "Код | Тема | Время | Преподаватель"
  r <- quickQuery' conn ("SELECT t.id,t.name,t.stime,l.name as lecture_name"
        ++ " FROM topic as t LEFT JOIN lecturer as l ON t.lecturer_id=l.id ORDER BY stime,name") []
  mapM_ (\x -> do
           let [id,name,time,lecture_name] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ (fromSql name) ++ " " 
                ++ (dateTimeStrFromSql time) ++ " " 
                ++ (fromSql lecture_name)
           return ()
         ) r

-- добавление темы
topic_add :: IConnection conn => conn -> IO ()
topic_add conn = do
  putStrLn "\nДобавление темы"
  m_topic <- get_topic conn 0
  case m_topic of
    Nothing -> return ()
    Just topic -> do
      insert_rec conn topic
      return ()
  
-- изменение темы
topic_edit :: IConnection conn => conn -> IO ()
topic_edit conn = do
  putStrLn "\nИзменение темы"
  putStrLn "Код темы: "
  id <- getInt
  r_topic <- find_topic conn id
  case r_topic of
  
    Just topic  -> do
      putStrLn $ "Изменяемая тема: " ++ show topic
      m_topic <- get_topic conn id
      case m_topic of
        Nothing -> return ()
        Just topic -> do
          update_rec conn topic
          return ()
      
    Nothing ->
      putStrLn "Тема не найдена"
  
-- удаление темы
topic_del :: IConnection conn => conn -> IO ()
topic_del conn = do
  putStrLn "Удаление темы"
  putStrLn "Код темы: "
  id <- getInt
  r_topic <- find_topic conn id
  case r_topic of
    Just topic -> do
      putStrLn $ "Тема: " ++ show topic
      delete_rec conn topic
      return ()
    Nothing ->
      putStrLn "Тема не найдена"
      

-- меню Темы
menu_topics :: IConnection conn => conn -> IO ()
menu_topics conn = do
  putStrLn "\n[Темы]"
  print_list [
       (1, "Список"),
       (2, "Выбрать"),
       (3, "Добавить"),
       (4, "Изменить"),
       (5, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_topics conn
        menu_topics conn
    "2" -> do
        topic_sel conn
        menu_topics conn
    "3" -> do
        topic_add conn
        menu_topics conn
    "4" -> do
        topic_edit conn
        menu_topics conn
    "5" -> do
        topic_del conn
        menu_topics conn
    "0" ->
        return ()
    otherwise -> 
        menu_topics conn
        
    
show_groups :: IConnection conn => conn -> IO ()
show_groups conn = do
  putStrLn "Группы:"
  putStrLn "Код | Номер группы"
  q <- prepare conn "SELECT id, numb FROM sgroup ORDER BY numb"
  execute q []
  results <- fetchAllRows q
  mapM_ (\x -> do
           let [id, numb] = x
           putStrLn $ (fromSql id) ++ " | "  ++ (fromSql numb)
           return ()
         ) results

group_add :: IConnection conn => conn -> IO ()
group_add conn = do
  putStrLn "\nДобавление группы"
  group <- get_group 0
  insert_rec conn group
  return ()
  
group_edit :: IConnection conn => conn -> IO ()
group_edit conn = do
  putStrLn "\nИзменение группы"
  putStrLn "Код группы: "
  id <- getInt
  r_group <- find_group conn id
  case r_group of
    Just group  -> do
      putStrLn $ "Изменяемая группа: " ++ show group
      group <- get_group id
      update_rec conn group
      return ()
    Nothing ->
      putStrLn "Группа не найдена"
  
group_del :: IConnection conn => conn -> IO ()
group_del conn = do
  putStrLn "Удаление группы"
  putStrLn "Код группы: "
  id <- getInt
  r_group <- find_group conn id
  case r_group of
    Just group  -> do
      putStrLn $ "Группа: " ++ show group
      delete_rec conn group
      return ()
    Nothing ->
      putStrLn "Группа не найдена"

-- меню Группы
menu_groups :: IConnection conn => conn -> IO ()
menu_groups conn = do
  putStrLn "\n[Группы]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_groups conn
        menu_groups conn
    "2" -> do
        group_add conn
        menu_groups conn
    "3" -> do
        group_edit conn
        menu_groups conn
    "4" -> do
        group_del conn
        menu_groups conn
    "0" ->
        return ()
    otherwise -> 
        menu_groups conn

-- показать список студентов
show_students :: IConnection conn => conn -> Group -> IO ()
show_students conn group = do
  putStrLn $ "Студенты группы " ++ grNumb group ++ ":"
  putStrLn "Код | Фамилия | Имя | Отчество | Дата рождения"
  r <- quickQuery' conn ("SELECT id,name,surname,middlename,birthdate"
        ++ " FROM student WHERE sgroup_id=? ORDER BY name,surname,middlename")
        [toSql (rec_id group)]
  mapM_ (\x -> do
           let [id,name,surname,middlename,birthdate] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ (fromSql name) ++ " " 
                ++ (fromSql surname) ++ " " 
                ++ (fromSql middlename)
                ++ " | "  ++ (dateStrFromSql birthdate)
           return ()
         ) r

-- добавление студента
student_add :: IConnection conn => conn -> Group -> IO ()
student_add conn group = do
  putStrLn "\nДобавление студента"
  student <- get_student 0 group
  insert_rec conn student
  return ()
  
-- изменение студента
student_edit :: IConnection conn => conn -> Group -> IO ()
student_edit conn group = do
  putStrLn "\nИзменение студента"
  putStrLn "Код студента: "
  id <- getInt
  r_student <- find_student_gr conn id group
  case r_student of
    Just student  -> do
      putStrLn $ "Изменяемый студент: " ++ show student
      student <- get_student id group
      update_rec conn student
      return ()
    Nothing ->
      putStrLn "Студент не найден"
  
-- удаление студента
student_del :: IConnection conn => conn -> Group -> IO ()
student_del conn group = do
  putStrLn "Удаление студента"
  putStrLn "Код студента: "
  id <- getInt
  r_student <- find_student_gr conn id group
  case r_student of
    Just student  -> do
      putStrLn $ "Студент: " ++ show student
      delete_rec conn student
      return ()
    Nothing ->
      putStrLn "Студент не найдена"

-- меню Студенты
menu_students :: IConnection conn => conn -> Group -> IO ()
menu_students conn group = do
  putStrLn $ "\n[Студенты группы " ++ grNumb group  ++ "]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_students conn group
        menu_students conn group
    "2" -> do
        student_add conn group
        menu_students conn group
    "3" -> do
        student_edit conn group
        menu_students conn group
    "4" -> do
        student_del conn group
        menu_students conn group
    "0" ->
         return ()
    otherwise ->
        menu_students conn group
      
-- выбор группы для меню Студенты      
group_sel :: IConnection conn => conn -> IO ()
group_sel conn = do
  putStrLn "Код группы студентов: "
  id <- getInt
  r_group <- find_group conn id
  case r_group of
    Just group  -> do
      menu_students conn group
    Nothing ->
      putStrLn "Группа не найдена"
         

-- показать список преподавателей
show_lects :: IConnection conn => conn -> IO ()
show_lects conn = do
  putStrLn "Преподаватели:"
  putStrLn "Код | Фамилия | Имя | Отчество | Дата рождения"
  r <- quickQuery' conn ("SELECT id,name,surname,middlename,birthdate"
        ++ " FROM lecturer ORDER BY name,surname,middlename") []
  mapM_ (\x -> do
           let [id,name,surname,middlename,birthdate] = x
           putStrLn $ (fromSql id) 
                ++ " | "  ++ (fromSql name) ++ " " 
                ++ (fromSql surname) ++ " " 
                ++ (fromSql middlename)
                ++ " | "  ++ (dateStrFromSql birthdate)
           return ()
         ) r

-- добавление преподавателя
lect_add :: IConnection conn => conn -> IO ()
lect_add conn = do
  putStrLn "\nДобавление преподавателя"
  lecturer <- get_lecturer 0
  insert_rec conn lecturer
  return ()
  
-- изменение преподавателя
lect_edit :: IConnection conn => conn -> IO ()
lect_edit conn = do
  putStrLn "\nИзменение преподавателя"
  putStrLn "Код преподавателя: "
  id <- getInt
  r_lecturer <- find_lecturer conn id
  case r_lecturer of
    Just lecturer  -> do
      putStrLn $ "Изменяемый преподаватель: " ++ show lecturer
      lecturer <- get_lecturer id
      update_rec conn lecturer
      return ()
    Nothing ->
      putStrLn "Преподаватель не найден"
  
-- удаление преподавателя
lect_del :: IConnection conn => conn -> IO ()
lect_del conn = do
  putStrLn "Удаление преподавателя"
  putStrLn "Код преподавателя: "
  id <- getInt
  r_lecturer <- find_lecturer conn id
  case r_lecturer of
    Just lecturer  -> do
      putStrLn $ "Преподаватель: " ++ show lecturer
      delete_rec conn lecturer
      return ()
    Nothing ->
      putStrLn "Преподаватель не найдена"
      

-- меню Преподаватели
menu_lects :: IConnection conn => conn -> IO ()
menu_lects conn = do
  putStrLn "\n[Преподаватели]"
  print_list [
       (1, "Список"),
       (2, "Добавить"),
       (3, "Изменить"),
       (4, "Удалить"),
       (0, "Назад")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
        show_lects conn
        menu_lects conn
    "2" -> do
        lect_add conn
        menu_lects conn
    "3" -> do
        lect_edit conn
        menu_lects conn
    "4" -> do
        lect_del conn
        menu_lects conn
    "0" ->
        return ()
    otherwise -> 
        menu_lects conn


-- Главное меню
menu :: IConnection conn => conn -> IO ()
menu conn = do
  putStrLn "\n[БД Индивидуальная работа]"
  print_list [
       (1, "Темы"),
       (2, "Группы"),
       (3, "Студенты"),
       (4, "Преподаватели"),
       (0, "Выход")
       ]
  putStr "> "
  item <- getLine
  case item of
    "1" -> do
         menu_topics conn
         menu conn
    "2" -> do
         menu_groups conn
         menu conn
    "3" -> do
         group_sel conn
         menu conn
    "4" -> do
         menu_lects conn
         menu conn
    "0" ->
         return ()
    otherwise ->
         menu conn


