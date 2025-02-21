# Упражнение 6


**N.B.** Може - даже е препоръчително - да използвате функциите `accumulate`, `map`, `filter` и `foldl/foldr`, когато това улеснява решението на задачата. Също така може да използвате всички фиункции, които сме дефинирали в упражненията до момента.


## Задачи

### Работа с числа

1. Дефинирайте функцията `(palindrome? n)`, която връща дали числото `n` е палиндром.

```scheme
(palindrome? 12321) ;; => #t
(palindrome? 1234)  ;; => #f
```

2. Дефинирайте функцията `(count-palindromes a b)`, която връща броя на числата в интервала [a .. b], който са палиндроми.

3. Дефинирайте функцията `(sum-primes n k)`, която намира сбора на първите `n` прости числа по-големи от `k`.

4. Разлагане на прости делители: Да се дефинира функцията `(prime-factors n)`, която приема целочисления аргумент n и връща списък от двойки от тип `(pi . ki)`, където `pi` e i-тия прост делител на `n`, a `ki` степента на `pi` във факторизацията на `n`.

```scheme
(prime-factors 10)  ;; => '((2 . 1) (5 . 1))          ;  10 = 2^1 * 5^1
(prime-factors 360) ;; => '((2 . 3) (3 . 2) (5 . 1))  ; 360 = 2^3 * 3^2 * 5^1
```

### Работа със списъци, матрици и асоциативни списъци

5. Нека е даден списък `l`; да се дефинират следните функции:
  - `(increasing? l)`, която проверява дали елементите на `l` са подредени в нарастващ ред
  - `(progression? l)`, която проверява дали елементите на `l` образуват аритметична прогресия
  - `(has-duplicates? l)`, която проверява дали `l` съдържа повтарящи се елементи

6. Да се дефинира фунцкия `(dedup l)`, която премахва повтарящите се елементи от списъка `l`.

7. Нека са дадени два списъка `l` и `s`, които не съдържат повтарящи се елементи (т.е. `l` и `s` са множества). Да се дефинират следните функции:
  - `(union l s)`, която връща тяхното обединение
  - `(intersection l s)`, която връща тяхното сечение
  - `(product l s)`, която връщя тяхното картезианско произведение

```scheme
(union '(1 2 3 4) '(1 3 5 7))         ;; => '(1 2 3 4 5 7)
(intersection '(1 2 3 4) '(1 3 5 7))  ;; => '(1 3)
(product '(1 2) '(3 5 1))             ;; => '((1 . 3) (1 . 5) (1 . 1) (2 . 3) (2 . 5) (2 . 1))
```

8. Дефинирайте функцията `(most-common l)`, която приема списъ `l` и връща най-често срещания в него елемент.
```scheme
(most-common '(1 2 3 2 2 1)) ;; => 2
```

9. Да се дефинира функциятя `(scalar-product xs ys)`, която приема два списъка `xs` и `ys` и връща тяхното скаларно произведение.

10. Дефинирайте функцията `(diagonal-product matrix)`, която приема числова квадратна матрица matrix и връща скаларното произведение на двата и диагонала.

```scheme
(diagonal-product '((1 0 1) (0 2 0) (3 0 3))) ;; => 14
(diagonal-product '((1 0 3) (0 2 0) (1 0 3))) ;; => 10
```

11. Да се дефинира функцията `(matrix-multiply m t)`, която приема матриците `m` и `t` (представени като списъци от списъци) и връща тяхното матрично произведение. (**Бонус**: Дефинирайте функцията _на един ред_).

12. Да се дефинира функция `(max-ordered-sublist lst)`, която намира най-дългия възходящо сортиран подсписък на списъка от числа `lst`.

```scheme
(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) ;; => '(2 4 6 8)
```

13. Дефинирайте функция `(replace lst dict)`, която получава списък `lst` и асоциативен списък `dict` и връща нов списък, в който елементите на `lst` са заменени с асоциацията им в `dict`, ако имa такава, a иначе не се променят.

```scheme
(replace '(1 2 3 4) '((1 . c) (4 . 1)))  ;; => '(c 2 3 1)
```

14. Дефинирайте функцията `(closest-point xys)`, която приема списък от точки в равнината (представени чрез двойки `(x . y)`) и връща едноаргументна функция, чиято стойност в дадена точка `p` e най-близкатадо `p` точка от `xys`.

```scheme
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(2 . 1))   ;; => '(0 . 0)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(8 . 9))   ;; => '(10 . 10)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(10 . 1))  ;; => '(10 . 0)
((closest-point '((0 . 0) (0 . 10) (10 . 0) (10 . 10))) '(1 . 10))  ;; => '(0 . 10)
```

15. Дефинирайте функцията `(flatten l)`, която приема произволно дълбок списък `l` и го превръща в "плосък".

```scheme
(flatten '(1 2 3))                ;; => '(1 2 3)
(flatten '(1 (2 3) (3 (4 (5)))))  ;; => '(1 2 3 4 5)
```

16. Нека е дадено роботче с първоначална позиция `(0 . 0)`, както и списък с ходове `moves` представени като двойки `(dx . dy)`. Дефинирайте следните функции:

  - `(position moves)`, която връща крайната позиция на робота след като изпълни всички ходове в `moves`.
  - `(distance moves)`, която връща дължината на пътя изминат от робота. За целта може да приемете, че по време на всеки ход, роботът се движи по права линия.


### Работа с графи

17. Дефинирайте функцията `(adjacency-list nodes edges)`, която приема списъс с върхове `nodes` и списък с ребра `edges` на даден ориентиран граф (в който всяко ребро е представено като двойка `(from . to)`) и връща списъка на наследниците на съответния граф.

18. Дефинирайте функцията `(path? edges nodes)`, която приема списъс с ребрата `edges` на даден ориентиран граф и списък от върхове `nodes` и връща дали списъкът nodes е път в графа описан от `edges`.

19. Дефинирайте функцията `(all-simple-paths adj-list start end)`, която приема списъс на наследниците `adj-list` на даден  ориентирант граф и два върхa `start` и `end` и връща всички прости пътища, които започват от `start` и завършват в `end`.
