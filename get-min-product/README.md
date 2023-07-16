# **Задание 1** 
Реализовать функцию get-min-product, которая принимает на вход произвольное количество векторов (необязательно каждый из которых числовой) и возвращает наименьшее значение, которое может получиться при перемножении всех чисел входного вектора, являющегося числовым, удовлетворяющего условию: среди входных векторов нет числового вектора, имеющего большую длину. Гарантируется, что среди входных векторов есть хотя бы один числовой.

## **Решение 1** (product1.clj) 
1) Реализовать функцию get-max-len, которая находит наибольшую длину вектора среди входных векторов.
2) С использованием get-max-len реализовать функцию get-numeric-vectors-of-max-len, которая из входных векторов оставляет только те векторы, которые имеют наибольшую длину и которые являются числовыми.
3) С использованием get-numeric-vectors-of-max-len реализовать функцию get-min-product.

Такое решение неверное: если строго наибольшую длину среди входных векторов имеет нечисловой вектор, то возникнет ошибка.

## **Решение 2** (product2.clj)
Сначала следует оставить только числовые векторы.