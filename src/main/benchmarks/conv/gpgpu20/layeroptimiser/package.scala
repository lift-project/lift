package benchmarks.conv.gpgpu20

package object layeroptimiser {
  val input_X = Array(Array(Array(
    Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
      Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
    Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
      Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
    Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
      Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
    Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
      Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
    Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
      Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
    Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
      Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
    Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
      Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
    Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
      Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f))),
    Array(
      Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
        Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
      Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
        Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
      Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
        Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
      Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
        Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
      Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
        Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
      Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
        Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
      Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
        Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
      Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
        Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f)))))

  val input_b = Array(0.0f, 1.0f, 2.0f)

  val input_K: Array[Array[Array[Array[Float]]]] =
    Array(
      Array(
        Array(Array(1, 0), Array(3, 0), Array(5, 0)),
        Array(Array(7, 0), Array(9, 0), Array(11, 0)),
        Array(Array(13, 0), Array(15, 0), Array(17, 0))),
      Array(
        Array(Array(0, 1), Array(0, 3), Array(0, 5)),
        Array(Array(0, 7), Array(0, 9), Array(0, 11)),
        Array(Array(0, 13), Array(0, 15), Array(0, 17))),
      Array(
        Array(Array(1, 0), Array(3, 0), Array(5, 0)),
        Array(Array(7, 0), Array(9, 0), Array(11, 0)),
        Array(Array(13, 0), Array(15, 0), Array(17, 0))))


  val gold: Array[Array[Array[Array[Array[Float]]]]] =
    Array(
      Array(
        Array(
          Array(
            Array(1029, 1110, 1191, 1272, 1353, 1434),
            Array(1677, 1758, 1839, 1920, 2001, 2082),
            Array(2325, 2406, 2487, 2568, 2649, 2730),
            Array(2973, 3054, 3135, 3216, 3297, 3378),
            Array(3621, 3702, 3783, 3864, 3945, 4026),
            Array(4269, 4350, 4431, 4512, 4593, 4674)),
          Array(
            Array(1030, 1111, 1192, 1273, 1354, 1435),
            Array(1678, 1759, 1840, 1921, 2002, 2083),
            Array(2326, 2407, 2488, 2569, 2650, 2731),
            Array(2974, 3055, 3136, 3217, 3298, 3379),
            Array(3622, 3703, 3784, 3865, 3946, 4027),
            Array(4270, 4351, 4432, 4513, 4594, 4675)),
          Array(
            Array(1031, 1112, 1193, 1274, 1355, 1436),
            Array(1679, 1760, 1841, 1922, 2003, 2084),
            Array(2327, 2408, 2489, 2570, 2651, 2732),
            Array(2975, 3056, 3137, 3218, 3299, 3380),
            Array(3623, 3704, 3785, 3866, 3947, 4028),
            Array(4271, 4352, 4433, 4514, 4595, 4676))),
        Array(
          Array(
            Array(1029, 1110, 1191, 1272, 1353, 1434),
            Array(1677, 1758, 1839, 1920, 2001, 2082),
            Array(2325, 2406, 2487, 2568, 2649, 2730),
            Array(2973, 3054, 3135, 3216, 3297, 3378),
            Array(3621, 3702, 3783, 3864, 3945, 4026),
            Array(4269, 4350, 4431, 4512, 4593, 4674)),
          Array(
            Array(1030, 1111, 1192, 1273, 1354, 1435),
            Array(1678, 1759, 1840, 1921, 2002, 2083),
            Array(2326, 2407, 2488, 2569, 2650, 2731),
            Array(2974, 3055, 3136, 3217, 3298, 3379),
            Array(3622, 3703, 3784, 3865, 3946, 4027),
            Array(4270, 4351, 4432, 4513, 4594, 4675)),
          Array(
            Array(1031, 1112, 1193, 1274, 1355, 1436),
            Array(1679, 1760, 1841, 1922, 2003, 2084),
            Array(2327, 2408, 2489, 2570, 2651, 2732),
            Array(2975, 3056, 3137, 3218, 3299, 3380),
            Array(3623, 3704, 3785, 3866, 3947, 4028),
            Array(4271, 4352, 4433, 4514, 4595, 4676)))))
}
