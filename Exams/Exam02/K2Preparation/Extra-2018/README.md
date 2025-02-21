**Задача 1.** Редицата на Racamán се дефинира по следния начин:
- a<sub>0</sub> = 0
- a<sub>i</sub> = a<sub>i-1</sub> – i, ако a<sub>i-1</sub> > i и a<sub>i</sub> ∉ {a<sub>0</sub>, ..., a<sub>i-1</sub>}
- a<sub>i</sub> = a<sub>i-1</sub> + i, иначе.

Да се дефинира функция **racaman**, която генерира потока от числа, представящ
редицата на Racamán.
```haskell
racaman -- => [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, … ]
```
---
**Задача 2.** Дадени са списък от едноместни числови функции **fs** и списък от
числа **xs**. Да се дефинира функция **checkId**, която проверява дали има две
(не непременно различни) функции **f** и **g** от списъка **fs**, така че
**f(g(x)) = x** за всяко **x ∈ xs**.
```haskell
checkID [(^2), sin, (+2), sqrt] [1..5] -- => True
checkID [abs, max 2, min 4] [-1..5] -- => False
```
---
**Задача 3.** Резултат от футболна среща се представя с наредена четворка от
вида **(t1, t2, g1, g2)**, където **t1** и **t2** са имената на двата отбора
(низове), а **g1** и **g2** са съответен брой отбелязани голове (цели
неотрицателни числа). Даден е списък от резултати от футболни срещи games.
1) Да се реализира функция **allTeams**, която по списъка games връща списък от
всички имена на всички отбори без повторения.
2) Да се реализира функция **teamScore**, която по списъка games и име на отбор
team връща наредена тройка от общ брой точки, сума от отбелязани голове и сума
от получени голове за отбора team. При победа отбор получава 3 точки, при равен
резултат — 1 точка, а при загуба — 0 точки.
3) Да се реализира функция **scoreBoard**, която по списъка games връща списък
от наредени четворки, по една за всеки отбор, съдържащи име на отбора, заедно с
неговите общ брой точки, отбелязани голове и получени голове. Списъкът да е
подреден съгласно следната наредба:
- отбор с повече точки е по-напред в наредбата
- при равен брой точки, отбор с по-висока голова разлика (дефинирана като
  разликата между отбелязани и получени голове) е по-напред в наредбата
- при равни брой точки и голова разлика, отбор с повече отбелязани голове е
  по-напред в наредбата
- редът на отбори с равни брой точки, голова разлика и отбелязани голове не е от
  значение.