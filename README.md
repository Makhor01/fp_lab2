## Хороше Максим Денисович P3325
# Лабораторная работа № 2
### PreBag


Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.
В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).
Требования:

Функции:

добавление и удаление элементов;
фильтрация;
отображение (map);
свертки (левая и правая);
структура должна быть моноидом.


Структуры данных должны быть неизменяемыми.
Библиотека должна быть протестирована в рамках unit testing.
Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
Структура должна быть полиморфной.
Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
Обратите внимание:

API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.
## Реализация
### Установка

**Убедитесь, что у вас установлен OCaml и dune. Если нет, выполните следующие команды:**
```Bash
bash
Copy
sudo apt-get install opam
opam init --auto-setup
opam switch create 4.14.0  # Установите нужную версию OCaml
eval $(opam env)
opam install dune alcotest qcheck qcheck-alcotest
```
**Клонируйте репозиторий:**
```Bash
bash
Copy
git clone https://github.com/Makhor01/fp_lab2.git
cd fp_lab2.git
```
**Соберите проект с помощью dune:**
```Bash
bash
Copy
dune build
```
### Кючевые моменты реализации
1. Тип данных для Prefix Tree

Структура данных "Bag" реализована как бинарное дерево, где каждый узел содержит:

- Ключ (элемент типа 'a).
- Количество вхождений ключа (целое число).
- Левый и правый дочерние узлы (также деревья).
```Ocaml
type 'a t =
  | Empty
  | Node of 'a * int * 'a t * 'a t  (* key, count, left, right *)
```
2. Неизменяемость

Все операции над структурой данных возвращают новое дерево, не изменяя исходное.

Пример:
```Ocaml
let rec add x = function
  | Empty -> Node (x, 1, Empty, Empty)
  | Node (key, count, left, right) when x < key -> Node (key, count, add x left, right)
  | Node (key, count, left, right) when x > key -> Node (key, count, left, add x right)
  | Node (key, count, left, right) -> Node (key, count + 1, left, right)
```

3. Добавление и удаление элементов

Добавление (add):
- Если элемент уже существует, увеличивается его счётчик.
- Если элемент отсутствует, создаётся новый узел.
Удаление (remove):
- Если счётчик элемента больше 1, он уменьшается.
- Если счётчик равен 1, узел удаляется, и дерево перестраивается.
Пример удаления:
```Ocaml
let rec remove x = function
  | Empty -> Empty
  | Node (key, count, left, right) when x < key -> Node (key, count, remove x left, right)
  | Node (key, count, left, right) when x > key -> Node (key, count, left, remove x right)
  | Node (_, count, left, right) ->
    if count > 1 then Node (x, count - 1, left, right)
    else match left, right with
      | Empty, _ -> right
      | _, Empty -> left
      | _ ->
        let rec min_key = function
          | Node (k, _, Empty, _) -> k
          | Node (_, _, l, _) -> min_key l
          | Empty -> failwith "Unexpected"
        in
        let min_right = min_key right in
        Node (min_right, 1, left, remove min_right right)
```

4. Фильтрация и отображение

Фильтрация (filter):
- Рекурсивно обходит дерево и создаёт новое дерево, содержащее только элементы, удовлетворяющие предикату.
Отображение (map):
- Применяет функцию к каждому элементу дерева и создаёт новое дерево с результатами.

Пример фильтрации:
```Ocaml
let rec filter pred = function
  | Empty -> Empty
  | Node (key, count, left, right) ->
    let new_left = filter pred left in
    let new_right = filter pred right in
    if pred key then Node (key, count, new_left, new_right)
    else
      match new_left, new_right with
      | Empty, _ -> new_right
      | _, Empty -> new_left
      | _ -> Node (key, count, new_left, new_right)
```

5. Операции над множествами

Объединение (union):
- Создаёт новое дерево, содержащее все элементы из двух деревьев.
Пересечение (intersection):
- Создаёт новое дерево, содержащее только общие элементы.
Разность (difference):
- Создаёт новое дерево, содержащее элементы из первого дерева, которых нет во втором.
Пример объединения:
```Ocaml
let rec union t1 t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | Node (key1, count1, left1, right1), Node (key2, count2, left2, right2) ->
    if key1 < key2 then
      Node (key1, count1, left1, union right1 t2)
    else if key1 > key2 then
      Node (key2, count2, left2, union right2 t1)
    else
      Node (key1, count1 + count2, union left1 left2, union right1 right2)
```
### Использование

Модуль PrefixTreeBag реализует интерфейс Bag, предоставляя следующие функции:

1. Добавление элемента: add x bag
2. Удаление элемента: remove x bag
3. Проверка наличия элемента: contains x bag
4. Фильтрация: filter pred bag
5. Отображение: map f bag
6. Свертка (левая и правая): fold_left f acc bag, fold_right f bag acc
7. Операции над множествами: union, intersection, difference
8. Преобразование в список и обратно: to_list bag, of_list lst
9. Сравнение структур: equals bag1 bag2

### Unit-тесты

Unit-тесты проверяют корректность работы отдельных функций, таких как add, remove, contains, filter, map, fold_left, fold_right, union, intersection, difference, to_list, of_list и equals.

### Property-based тесты

Property-based тесты проверяют свойства структуры данных, включая свойства моноида:

- Ассоциативность объединения: union (union a b) c = union a (union b c)
- Нейтральный элемент: union a empty = a и union empty a = a
- Коммутативность объединения: union a b = union b a
