module Main where

import Data.Monoid
import Data.Maybe (fromMaybe)

-- Маппинг букв на ноты
letterToNote :: Char -> String
letterToNote 'a' = "C4"
letterToNote 'b' = "D4"
letterToNote 'c' = "E4"
letterToNote 'd' = "F4"
letterToNote 'e' = "G4"
letterToNote 'f' = "A4"
letterToNote 'g' = "B4"
letterToNote 'h' = "C5"
letterToNote _   = "C4" -- Для остальных букв по умолчанию нота C4

-- Маппинг нот на буквы
noteToLetter :: String -> Maybe Char
noteToLetter "C4" = Just 'a'
noteToLetter "D4" = Just 'b'
noteToLetter "E4" = Just 'c'
noteToLetter "F4" = Just 'd'
noteToLetter "G4" = Just 'e'
noteToLetter "A4" = Just 'f'
noteToLetter "B4" = Just 'g'
noteToLetter "C5" = Just 'h'
noteToLetter _    = Nothing

-- Функция для конкатенации нот из текста
textToNotes :: String -> String
textToNotes [] = mempty  -- Нейтральный элемент — пустая строка
textToNotes (x:xs) = letterToNote x <> " " <> textToNotes xs

-- Функция для преобразования нот в текст
notesToText :: [String] -> String
notesToText [] = mempty  -- Нейтральный элемент — пустая строка
notesToText (x:xs) = fromMaybe ' ' (noteToLetter x) : notesToText xs

-- Главная программа
main :: IO ()
main = do
    putStrLn "Выберите режим работы программы:"
    putStrLn "1 - Преобразовать текст в музыкальную последовательность"
    putStrLn "2 - Преобразовать музыкальную последовательность в текст"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Введите текст для преобразования в музыкальную последовательность:"
            input <- getLine
            let notes = textToNotes input
            putStrLn $ "Музыкальная последовательность: " ++ notes
        "2" -> do
            putStrLn "Введите музыкальную последовательность для преобразования в текст (например, C4 D4 E4):"
            input <- getLine
            let noteList = words input  -- Разбиваем строку по пробелам на список нот
            let text = notesToText noteList
            putStrLn $ "Текст: " ++ text
        _ -> putStrLn "Неверный выбор. Попробуйте снова."

