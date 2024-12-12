**Задача 1.** Да се напише функция `findColumns`, която по дадена матрица от числа намира броя на колоните, за които е вярно, че всичките им елементи се срещат в някой от редовете на матрицата.

```haskell
findColumns [[1,4,3],[4,5,6],[7,4,9]] -- => 1
```

**Задача 2.** Ако **f** и **g** са унарни числови функции, а **h** е бинарна числова функция, то дефинираме функцията
**f{h}g** по следния начин: **(f{h}g) (x) := h(f(x), g(x))**

Дадени са две цели числа **a < b** и два списъка **uns** и
**bins**, съответно с унарни и бинарни функции. Да се напише функция **check**, която проверява дали има такива функции
**f** и **g** от списъка **uns** и **h** от списъка **bins** така, че **f{h}g** да съвпада с някоя от функциите в
**uns** върху всички цели числа в интервала **[a;b]**.

**Упътване**: използвайте помощна функция от по-висок ред
**combine**, която по дадени **f**, **g** и **h** връща така дефинираната **f{h}g**.

```haskell
check 1 9 [(+1),(-1),(-1).(^2)] [(*)] -- => True
```

**Задача 3.** Растение се описва с наредена тройка от име (низ), минимална и максимална температура, в която вирее
(цели числа градуси). Да се напише функция **garden**, която по списък от растения намира интервал от стойности на
температура, в която максимален брой растения могат да виреят, заедно с имената на тези растения.

```haskell
garden [("peas", 5, 25), ("beans", 3, 15), ("cocoa", 20, 30)] -- => ((20, 25), ["peas", "cocoa"])
```

**Задача 4.**  Даден е граф **g**, представен със списък от наследници и връх **u** в графа. Да се напише функция
**maxPath**, която намира най-дългия ацикличен път в **g**, започващ от върха **u**.
```haskell
maxPath [[1,2,4],[2,3],[3,2],[4]] 1 -- => [1,2,3]
```