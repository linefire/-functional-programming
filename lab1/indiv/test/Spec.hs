import Indiv
import Test.HUnit
import Database.HDBC
import Database.HDBC.ODBC


-- очистка таблиц тестовой базы данных
clear_db :: IConnection conn => conn -> IO ()
clear_db conn = do
  mapM_ (\tab -> do
          runRaw conn $ "DELETE FROM " ++ tab
          runRaw conn $ "ALTER TABLE " ++ tab ++ " AUTO_INCREMENT=1"
          return ()
    ) ["task", "question", "material", "topic", "lecturer", "student", "sgroup"]
  commit conn
  
  
main :: IO Counts
main = do

  -- подключение к базе данных
  --conn <- connectODBC "DSN=indiv_test"
  conn <- connectODBC "Driver={MySQL ODBC 8.0 Unicode Driver};Server=localhost;Port=3306;Uid=root;Pwd=;Database=indiv_test;Charset=utf8"
  
  clear_db conn
  
  result <- runTestTT $ TestList [
    
    TestCase $ assertEqual "sql -> data"
      "11.04.2001"
      (dateStrFromSql $ SqlString "2001-04-11"),

    TestCase $ assertEqual "sql -> datatime"
      "11.04.2001 03:10"
      (dateTimeStrFromSql $ SqlString "2001-04-11 03:10:00"),
      
    TestCase $ assertEqual "data -> sql"
      (SqlString "2001-04-11")
      (dateToSql $ strToDate "11.04.2001"),

    TestCase $ assertEqual "datatime -> sql"
      (SqlString "2001-04-11 03:10:00")
      (dateTimeToSql $strToDateTime "11.04.2001 03:10"),

    TestCase $ assertEqual "boolToSql True"
      1
      ((fromSql (boolToSql True)) :: Int),

    TestCase $ assertEqual "boolToSql False"
      0
      ((fromSql (boolToSql False)) :: Int),
      
    -- group
    TestCase $ do 
       insert_rec conn (Group{ grId = 0, grNumb = "группа1" })
         
       x1 <- find_group conn 2
       assertEqual "group 1" Nothing x1 
         
       insert_rec conn (Group{ grId = 0, grNumb = "группа2" })
        
       x2 <- find_group conn 2
       assertEqual "group 2" (Just Group{ grId = 2, grNumb = "группа2" }) x2
       
       update_rec conn (Group{ grId = 2, grNumb = "группа2б" })

       x3 <- find_group conn 2
       assertEqual "group 3" (Just Group{ grId = 2, grNumb = "группа2б" }) x3

       delete_rec conn Group{ grId = 2, grNumb = "группа2б" }
       
       x4 <- find_group conn 2
       assertEqual "group 4" Nothing x4

       insert_rec conn (Group{ grId = 0, grNumb = "группа3" })
       
       return (),
       
    -- student
    TestCase $ do 
       insert_rec conn Student{ stId = 0, stName = "Фамилия1", stSurname = "Имя1",
                stMiddlename = "Отчество1", stBirthdate = (strToDate "18.03.2003"),
                stGroupId = 3 }
                
       let student1 = Student{ stId = 1, stName = "Фамилия1", stSurname = "Имя1",
                stMiddlename = "Отчество1", stBirthdate = strToDate("18.03.2003"),
                stGroupId = 3 }
         
       x1 <- find_student_gr conn 1 Group{ grId = 3, grNumb = "группа3" }
       assertEqual "student 1" (Just student1) x1 

       x2 <- find_student_gr conn 1 Group{ grId = 1, grNumb = "группа1" }
       assertEqual "student 2" Nothing x2 

       x3 <- find_student conn 1
       assertEqual "student 3" (Just student1) x3
       
       let student2 = Student{ stId = 1, stName = "Фамилия1б", stSurname = "Имя1б",
                stMiddlename = "Отчество1б", stBirthdate = strToDate("19.02.2004"),
                stGroupId = 3 }
                
       update_rec conn student2

       x4 <- find_student conn 1
       assertEqual "student 4" (Just student2) x4

       delete_rec conn student2
       
       x5 <- find_student conn 1
       assertEqual "student 5" Nothing x5
                
       insert_rec conn Student{ stId = 0, stName = "Фамилия2", stSurname = "Имя2",
                stMiddlename = "Отчество2", stBirthdate = (strToDate "18.03.2003"),
                stGroupId = 3 }
       
       return (),

    -- lecturer
    TestCase $ do 
       insert_rec conn Lecturer{ lcId = 0, lcName = "Фамилия11", lcSurname = "Имя11",
                lcMiddlename = "Отчество11", lcBirthdate = (strToDate "18.03.1963") }
                
       let lecturer1 = Lecturer{ lcId = 1, lcName = "Фамилия11", lcSurname = "Имя11",
                lcMiddlename = "Отчество11", lcBirthdate = strToDate("18.03.1963") }
         
       x1 <- find_lecturer conn 1
       assertEqual "lecturer 1" (Just lecturer1) x1
       
       let lecturer2 = Lecturer{ lcId = 1, lcName = "Фамилия11б", lcSurname = "Имя11б",
                lcMiddlename = "Отчество11б", lcBirthdate = strToDate("19.02.1964") }
                
       update_rec conn lecturer2

       x2 <- find_lecturer conn 1
       assertEqual "lecturer 2" (Just lecturer2) x2

       delete_rec conn lecturer2
       
       x3 <- find_lecturer conn 1
       assertEqual "lecturer 3" Nothing x3
                
       insert_rec conn Lecturer{ lcId = 0, lcName = "Фамилия21", lcSurname = "Имя21",
                lcMiddlename = "Отчество21", lcBirthdate = (strToDate "18.03.1963") }
       
       return (),

    -- topic
    TestCase $ do 
       insert_rec conn Topic{ tpId = 0, tpName = "Тема1", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
                
       let topic1 = Topic{ tpId = 1, tpName = "Тема1", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
         
       x1 <- find_topic conn 1
       assertEqual "topic 1" (Just topic1) x1
       
       let topic2 = Topic{ tpId = 1, tpName = "Тема2", 
               tpTime = (strToDateTime "01.05.2021 11:36"), tpLecturerId = 2 }
                
       update_rec conn topic2

       x2 <- find_topic conn 1
       assertEqual "topic 2" (Just topic2) x2

       delete_rec conn topic2
       
       x3 <- find_topic conn 1
       assertEqual "topic 3" Nothing x3
                
       insert_rec conn Topic{ tpId = 0, tpName = "Тема2", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
       
       return (),
    
    -- task
    TestCase $ do 
       let topic = Topic{ tpId = 2, tpName = "Тема2", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
    
       insert_rec conn Task{ tkId = 0, tkName = "Задача1", tkComplete = False, tkTopicId = 2 }
               
       let task1 = Task{ tkId = 1, tkName = "Задача1", tkComplete = False, tkTopicId = 2 }
         
       x1 <- find_task conn 1 topic
       assertEqual "task 1" (Just task1) x1
       
       let task2 = Task{ tkId = 1, tkName = "Задача1б", tkComplete = True, tkTopicId = 2 }
                
       update_rec conn task2

       x2 <- find_task conn 1 topic
       assertEqual "task 2" (Just task2) x2

       delete_rec conn task2
       
       x3 <- find_task conn 1 topic
       assertEqual "task 3" Nothing x3
                
       insert_rec conn Task{ tkId = 0, tkName = "Задача2", tkComplete = True, tkTopicId = 2 }
       
       return (),

    -- question
    TestCase $ do
       let topic = Topic{ tpId = 2, tpName = "Тема2", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
    
       insert_rec conn Question{ quId = 0, quQuestion = "Вопрос1", quAnswer = "Ответ1",
                                 quTopicId = 2, quStudentId = 2 }
               
       let question1 = Question{ quId = 1, quQuestion = "Вопрос1", quAnswer = "Ответ1",
                                 quTopicId = 2, quStudentId = 2 }
         
       x1 <- find_question conn 1 topic
       assertEqual "question 1" (Just question1) x1
       
       let question2 = Question{ quId = 1, quQuestion = "Вопрос1б", quAnswer = "Ответ1б",
                                 quTopicId = 2, quStudentId = 2 }
                
       update_rec conn question2

       x2 <- find_question conn 1 topic
       assertEqual "question 2" (Just question2) x2

       delete_rec conn question2
       
       x3 <- find_question conn 1 topic
       assertEqual "question 3" Nothing x3
                
       insert_rec conn Question{ quId = 0, quQuestion = "Вопрос2", quAnswer = "Ответ2",
                                 quTopicId = 2, quStudentId = 2 }
       
       return (),
       
    -- material
    TestCase $ do
       let topic = Topic{ tpId = 2, tpName = "Тема2", 
               tpTime = (strToDateTime "01.05.2020 11:35"), tpLecturerId = 2 }
    
       insert_rec conn Material{ mtId = 0, mtName = "Материал1", mtTopicId = 2 }
               
       let material1 = Material{ mtId = 1, mtName = "Материал1", mtTopicId = 2 }
         
       x1 <- find_material conn 1 topic
       assertEqual "material 1" (Just material1) x1
       
       let material2 = Material{ mtId = 1, mtName = "Материал1б", mtTopicId = 2 }
                
       update_rec conn material2

       x2 <- find_material conn 1 topic
       assertEqual "material 2" (Just material2) x2

       delete_rec conn material2
       
       x3 <- find_material conn 1 topic
       assertEqual "material 3" Nothing x3
                
       insert_rec conn Material{ mtId = 2, mtName = "Материал2", mtTopicId = 2 }
       
       return ()
      
    ]

  clear_db conn

  disconnect conn
  return result
  
  