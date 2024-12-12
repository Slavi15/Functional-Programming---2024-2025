# 10. Списъци и Низове

1. Символи: символите в Haskell са от тип `Char`

  - Полезни функции върху символи в Прелюдията:

  ```haskell
  ord c -- връща ASCII кода на символа c. Пример: ord 'q' = 113
  chr n -- връща символа с ASCII код n. Пример: chr 113 = 'q'
  ```

  - [`import Data.Char`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Char.html) ви дава достъп до следните полезни функции:

  ```haskell
  toUpper c -- обръща буквата c в главна.
  -- toUpper 'Q' = 'Q';      toUpper 'q' = 'Q';      toUpper '1' = '1'

  toLower c -- обръща буквата c в малка.
  -- toLower 'Q' = 'q';      toLower 'q' = 'q';      toLower '1' = '1'

  isUpper c -- проверява дали буквата c e главна.
  -- isUpper 'Q' = True;     isUpper 'q' = False;    isUpper '1' = False

  isLower c -- проверява дали буквата c e малка.
  -- isLower 'Q' = False;    isLower 'q' = True;     isLower '1' = False

  isAlpha c -- проверява дали символът c е буква.
  -- isAlpha 'Q' = True;     isAlpha 'q' = True;     isAlpha '1' = False

  isDigit c -- проверява дали символът c e цифра.
  -- isDigit 'Q' = False;    isDigit 'q' = False;    isDigit '1' = True

  isSpace c -- проверява дали символът c e интервал.
  -- isSpace ' ' = True;     isSpace '\t' = True;    isSpace 'q' = False

  isAlphaNum c -- проверява дали символът c е буква или цифра.
  -- isAlphaNum ';' = False; isAlphaNum 'q' = True;  isAlphaNum '1' = True
  ```

2. Символни низове: символният низ в Haskell е просто списък от символи, т.е. `type String = [Char]`. Следователно всички функции работещи върху списъци, работят и върху символни низове!

3. Функциите `read` и `show`:

  - `show a` - връща стринговата репрезентация на `а`.

  ```haskell
  show 123        -- "123"
  show "123"      -- "\"123\""
  show [1, 2, 3]  -- "[1,2,3]"
  ```

  - `read s` - конвертира символния низ `s` до стойност.
  ```haskell
  read "123" :: Integer       -- 123
  read "[1,2,3]" :: [Integer] -- [1, 2, 3]
  ```

4. Модулът [`Data.List`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html#v:nub), който включва доста полезни функии върху списъци (напр. `sort`, )


## Задачи:

1. Да се дефинира функцията `whisper str`, която обръща в малки букви всички символи на низа `str`.

```haskell
whisper "BANANA" -- "banana"
```

2. Да се дефинира функцията `removeSpaces str`, която премахва всички интервали от символния низ `str`.

```haskell
removeSpaces "The Sound And The Fury" -- "TheSoundAndTheFury"
```

3. Напишете функцията switchCaps str, която обръща малките букви в големи, а големите в малки.

```haskell
switchCaps "baNaNA" -- "BAnAna"
```

4. Дефинирайте функциите `encrypt n str` и `decrypt n str`, които имплементират Цезаров шифър с отместване `n`.

```haskell
encrypt 1 "ABC"   -- "BCD"
encrypt 2 "AXYZ"  -- "CZAB"
decrypt 1 "BCD"   -- "ABC"
decrypt 1 "CZAB"  -- "AXYZ"
```

5. Напишете функцията `joinWords c strs`, която слива няколко думи в една, използвайки за разделител символ `c`.

```haskell
joinWords ' ' ["The", "Sound", "of", "Silence"] -- "The Sound of Silence"
joinWords ',' ["One", "Two", "Three", "Four"]   -- "One,Two,Three,Four"
```

6. Трансформация на Бъроус-Уилър ([Burrows-Wheeler Transform](https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform))

Трансформацията на Бъроус-Уилър, е трансформация върху символни низове, която има интересното свойство да групира еднаквите символи в низа близо един до друг. Поради тази причина, тя се използвана понякога като предварителна стъпка в алгоритмите за компресия на данни, както и в биоинформатиката.

Алгоритъмът за намирането ѝ е следния:
  1. Генерираме всички ротации на входния низ, което ни дава списък от низове.
  2. Сортираме списъка лексикографски. (Използвайте функцията `sort` от `Data.List`)
  3. Взимаме последния символ от всеки от редовете.

_Пример_:
```
"BANANA" -- rotations --> [ "BANANA", -- sort --> [ "ABANAN", -- last symbol --> "NNBAAA"
                            "ANANAB",               "ANABAN",
                            "NANABA",               "ANANAB",
                            "ANABAN",               "BANANA",
                            "NABANA",               "NABANA",
                            "ABANAN" ]              "NANABA" ]
```

7. Дефинирайте функцията `indices x xs`, която връща всички индекси на елементи от списъка `xs`, чиято стойност е равна на `x`.

```haskell
indices 1 [1, 2, 3, 1, 4] -- [0, 3]
indices 1 []              -- []
```

8. Дефинирайте функцията `lastIndex x xs`, която приема 2 аргумента - елемент `x` и списък `xs`- и връща индекса  (0-базиран) на последното срещане на `x` в `xs`. Ако `x` не се среща в `xs`, функцията връща грешка.

```haskell
lastIndex 1 [1, 2, 7, 1, 5, 4]  -- 3
lastIndex 7 [3, 1, 7, 5]        -- 2
lastIndex 1 []                  -- error "not in list"
lastIndex 3 [2, 4, 7]           -- error "not in list"
```

9. Брой на повторенията на най-малкото число в списъка: дефинирайте функцията `countMin xs`, която намира броя на срещанията на най-малкия елемент на списъка `xs` в него. Списъкът `xs` е несортиран и e съставен само от положителни цели числа.

```haskell
countMin [1, 2, 1, 1, 5, 3] -- 3
countMin [3, 4, 2]          -- 1
countMin []                 -- 0
```

10. Просто пренареждане: Дефинирайте функцията `primeReorder xs`, която получава списък `xs` и връща нов списък `ys`. В началото на `ys` трябва да са елементите, които са били с индекс просто число в `xs`. След тях трябва да са всички останали.Индексирането в `xs` започва от 2.

```haskell
primeReorder [2,3,4,5,6]  -- [2,3,5,4,6]
primeReorder "abcd"       -- "abdc"
```

11. Да се дефинира функция `dedup xs`, която премахва всички дубликати в `xs`.

```haskell
dedup [1, 2, 1]          -- [1, 2]
dedup [1, 3, 7, 3, 5, 1] -- [1, 3, 7, 5]
```

12. Mergesort:

  а). Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред и ги обединява в един списък, чийто елементи също са подредени в нарастващ ред.

  ```haskell
  merge [1, 3, 7] [2, 4, 6] -- [1, 2, 3, 4, 6, 7]
  ```

  б). Използвайте функцията от предишната подточка и идеята, за да напишете функция `mergesort xs`, която приема списък `xs` и връща списък с елементите на `xs` сортирани в нарастващ ред.

  ```haskell
  mergesort [2, 1, 3, 7, -16, 5] = [-16, 1, 2, 3, 5, 7]
  ```

13. Дефинирайте функцията `subsets xs`, която връща списък с всички подсписъци на списъка `xs`.

```haskell
subsets [] = [[]]
subsets [1, 2] = [[1, 2], [1], [2], []]
```

14. Дефинирайте функцията `pick k xs`, която връща списък с всички възможни избора на `k` елемента от списъка `xs`.

> [!NOTE]
> В случая примемаме, че изборът [1, 2] е неразличим от [2, 1]. Приемаме също, че в `xs` няма повтарящи се елементи (`xs` е множество).

```haskell
pick 2 [1, 2, 3] -- [[1, 2], [1, 3], [2, 3]]
```

15. Напишете функцията `maximize`, която получава непразен списък от едноместни числови функции и връща нова едноместна числова функция на аргумент `x`, която дава стойността `f x` на тази фунция `f` от списъка, за която числото `f x` е най-голямо по абсолютна стойност.

```haskell
maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5                  -- 1.5
maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2) -- 16
```

16. Напишете функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

```haskell
compose [(+1), (2*)] 7        -- (2 * 7) + 1 = 15
compose [(+1), (+1), (+1)] 7  -- 10
```

17. Дефинирайте безкрайния списък `facts`, който съдържа стойностите на `n!` за всяко `n`

```haskell
take 10 $ facts -- [1,2,6,24,120,720,5040,40320,362880,3628800]
```

18. Дефинирайте безкрайния списък `points`, който съдържа всички точки в равнината, чиито координати са цели числа

```haskell
take 10 $ points -- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
```