
#include "gtest/gtest.h"
#include <cmath>

int pow(int a, int b) {
  return std::pow(a,b);
}

int v_a_163 = 12;
int v_b_164 = 57;
int v_c_165 = 2;
int v_d_166 = 4;


TEST(ArithExpr, Test1) {
  int res = v_a_163;
  res -= v_a_163;
  res *= 7;
  res -= 10;
  res += 4;
  res %= 13;
  res /= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-6) / (v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test2) {
  int res = 13;
  res *= 15;
  res += 14;
  res /= 6;
  res *= v_a_163;
  res *= v_b_164;
  res  = pow(res,(v_d_166 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((34*v_a_163*v_b_164),(v_d_166 % (3)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 23256) << "Wrong result after substitution";
}

TEST(ArithExpr, Test3) {
  int res = 1;
  res += v_a_163;
  res /= v_a_163;
  res -= 13;
  res  = pow(res,(16 % 3));
  res /= 19;
  res += 5;

  // Partially evaluated expression:
  EXPECT_EQ(res, (5+(((-12+((1) / (v_a_163)))) / (19)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 5) << "Wrong result after substitution";
}

TEST(ArithExpr, Test4) {
  int res = v_a_163;
  res += v_a_163;
  res /= 15;
  res *= 20;
  res *= 19;
  res %= v_b_164;
  res *= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (v_b_164*((380*(((2*v_a_163)) / (15))) % (v_b_164)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 2166) << "Wrong result after substitution";
}

TEST(ArithExpr, Test5) {
  int res = 3;
  res += v_d_166;
  res -= 16;
  res *= v_c_165;
  res  = pow(res,(1 % 3));
  res += 2;
  res -= 11;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-9+(-13*v_c_165)+(v_c_165*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -27) << "Wrong result after substitution";
}

TEST(ArithExpr, Test6) {
  int res = v_b_164;
  res %= 15;
  res  = pow(res,(v_a_163 % 3));
  res  = pow(res,(8 % 3));
  res -= 14;
  res /= 18;
  res *= 10;

  // Partially evaluated expression:
  EXPECT_EQ(res, (10*(((-14+pow((v_b_164 % (15)),(2*(v_a_163 % (3)))))) / (18)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test7) {
  int res = 4;
  res -= 17;
  res *= v_a_163;
  res += 9;
  res  = pow(res,(6 % 3));
  res  = pow(res,(5 % 3));
  res  = pow(res,(v_c_165 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, 1) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test8) {
  int res = 9;
  res  = pow(res,(4 % 3));
  res -= 16;
  res *= 15;
  res += 18;
  res /= v_a_163;
  res -= 18;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-18+((-87) / (v_a_163)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -25) << "Wrong result after substitution";
}

TEST(ArithExpr, Test9) {
  int res = 17;
  res %= 4;
  res  = pow(res,(v_b_164 % 3));
  res += 18;
  res *= 16;
  res %= 11;
  res  = pow(res,(9 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, 1) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test10) {
  int res = v_c_165;
  res += v_c_165;
  res -= 10;
  res  = pow(res,(v_c_165 % 3));
  res += v_b_164;
  res -= v_a_163;
  res += 7;

  // Partially evaluated expression:
  EXPECT_EQ(res, (7+v_b_164+(-1*v_a_163)+pow((-10+(2*v_c_165)),(v_c_165 % (3))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 88) << "Wrong result after substitution";
}

TEST(ArithExpr, Test11) {
  int res = v_c_165;
  res %= v_c_165;
  res -= 18;
  res -= 15;
  res /= v_a_163;
  res -= 4;
  res += 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-3+((-33) / (v_a_163)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -5) << "Wrong result after substitution";
}

TEST(ArithExpr, Test12) {
  int res = 10;
  res  = pow(res,(1 % 3));
  res %= 17;
  res -= 7;
  res -= 1;
  res *= 20;
  res -= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (40+(-1*v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -17) << "Wrong result after substitution";
}

TEST(ArithExpr, Test13) {
  int res = v_c_165;
  res -= v_a_163;
  res -= 3;
  res /= v_c_165;
  res *= 19;
  res += 2;
  res -= 6;

  // Partially evaluated expression:
  EXPECT_EQ(res, (15+(19*(((-3+(-1*v_a_163))) / (v_c_165))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -118) << "Wrong result after substitution";
}

TEST(ArithExpr, Test14) {
  int res = v_d_166;
  res -= 12;
  res -= 18;
  res  = pow(res,(v_b_164 % 3));
  res -= 12;
  res *= v_c_165;
  res -= 10;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-10+(-12*v_c_165)+(v_c_165*pow((-30+v_d_166),(v_b_164 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -32) << "Wrong result after substitution";
}

TEST(ArithExpr, Test15) {
  int res = v_d_166;
  res  = pow(res,(9 % 3));
  res -= 10;
  res %= 2;
  res /= 9;
  res -= v_c_165;
  res *= v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-1*v_c_165*v_d_166)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -8) << "Wrong result after substitution";
}

TEST(ArithExpr, Test16) {
  int res = 11;
  res *= 8;
  res -= 11;
  res -= v_d_166;
  res %= 14;
  res %= 7;
  res %= 7;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((77+(-1*v_d_166)) % (14)) % (7))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 3) << "Wrong result after substitution";
}

TEST(ArithExpr, Test17) {
  int res = 19;
  res += 4;
  res -= 10;
  res  = pow(res,(3 % 3));
  res *= 20;
  res  = pow(res,(7 % 3));
  res += v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, (20+v_a_163)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 32) << "Wrong result after substitution";
}

TEST(ArithExpr, Test18) {
  int res = 18;
  res += 11;
  res *= v_d_166;
  res += v_d_166;
  res -= v_d_166;
  res += 16;
  res *= 5;

  // Partially evaluated expression:
  EXPECT_EQ(res, (80+(145*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 660) << "Wrong result after substitution";
}

TEST(ArithExpr, Test19) {
  int res = v_b_164;
  res -= 3;
  res *= 6;
  res -= v_c_165;
  res  = pow(res,(16 % 3));
  res  = pow(res,(16 % 3));
  res %= 2;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-18+(6*v_b_164)+(-1*v_c_165)) % (2))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test20) {
  int res = v_a_163;
  res -= v_a_163;
  res -= 9;
  res *= 8;
  res -= v_b_164;
  res /= v_a_163;
  res *= 4;

  // Partially evaluated expression:
  EXPECT_EQ(res, (4*(((-72+(-1*v_b_164))) / (v_a_163)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -40) << "Wrong result after substitution";
}

TEST(ArithExpr, Test21) {
  int res = 17;
  res += 10;
  res -= v_b_164;
  res  = pow(res,(v_d_166 % 3));
  res -= v_a_163;
  res *= 14;
  res /= 4;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((((-7*v_a_163)+(7*pow((27+(-1*v_b_164)),(v_d_166 % (3)))))) / (2))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -147) << "Wrong result after substitution";
}

TEST(ArithExpr, Test22) {
  int res = 20;
  res  = pow(res,(14 % 3));
  res  = pow(res,(v_a_163 % 3));
  res %= v_b_164;
  res *= 10;
  res += 6;
  res -= 19;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-13+(10*(pow(400,(v_a_163 % (3))) % (v_b_164))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -3) << "Wrong result after substitution";
}

TEST(ArithExpr, Test23) {
  int res = 5;
  res %= 14;
  res  = pow(res,(9 % 3));
  res %= 14;
  res  = pow(res,(16 % 3));
  res  = pow(res,(5 % 3));
  res -= 5;

  // Partially evaluated expression:
  EXPECT_EQ(res, -4) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -4) << "Wrong result after substitution";
}

TEST(ArithExpr, Test24) {
  int res = v_c_165;
  res  = pow(res,(18 % 3));
  res *= v_a_163;
  res /= 10;
  res /= 12;
  res /= 17;
  res /= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((v_a_163) / ((2040*v_b_164)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test25) {
  int res = 9;
  res -= 6;
  res  = pow(res,(6 % 3));
  res *= v_c_165;
  res %= 5;
  res -= 4;
  res %= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-4+(v_c_165 % (5))) % (v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -2) << "Wrong result after substitution";
}

TEST(ArithExpr, Test26) {
  int res = 8;
  res -= v_b_164;
  res += 13;
  res += v_b_164;
  res  = pow(res,(v_a_163 % 3));
  res *= v_d_166;
  res *= v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, (v_a_163*v_d_166*pow(21,(v_a_163 % (3))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 48) << "Wrong result after substitution";
}

TEST(ArithExpr, Test27) {
  int res = v_b_164;
  res %= v_b_164;
  res *= 1;
  res -= 20;
  res *= v_c_165;
  res -= v_c_165;
  res /= v_c_165;

  // Partially evaluated expression:
  EXPECT_EQ(res, -21) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -21) << "Wrong result after substitution";
}

TEST(ArithExpr, Test28) {
  int res = 11;
  res -= 6;
  res += 4;
  res -= 20;
  res -= 2;
  res += 17;
  res += 16;

  // Partially evaluated expression:
  EXPECT_EQ(res, 20) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 20) << "Wrong result after substitution";
}

TEST(ArithExpr, Test29) {
  int res = 3;
  res += v_c_165;
  res  = pow(res,(13 % 3));
  res  = pow(res,(17 % 3));
  res /= 9;
  res += v_d_166;
  res += 7;

  // Partially evaluated expression:
  EXPECT_EQ(res, (7+v_d_166+((pow((3+v_c_165),2)) / (9)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 13) << "Wrong result after substitution";
}

TEST(ArithExpr, Test30) {
  int res = v_d_166;
  res *= 5;
  res *= 10;
  res += 17;
  res -= 17;
  res /= 11;
  res += 4;

  // Partially evaluated expression:
  EXPECT_EQ(res, (4+(((50*v_d_166)) / (11)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 22) << "Wrong result after substitution";
}

TEST(ArithExpr, Test31) {
  int res = 2;
  res += 16;
  res /= 5;
  res *= v_d_166;
  res += v_b_164;
  res  = pow(res,(v_d_166 % 3));
  res -= 19;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-19+pow((v_b_164+(3*v_d_166)),(v_d_166 % (3))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 50) << "Wrong result after substitution";
}

TEST(ArithExpr, Test32) {
  int res = v_c_165;
  res -= v_a_163;
  res -= 13;
  res /= 8;
  res  = pow(res,(11 % 3));
  res *= v_a_163;
  res  = pow(res,(8 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((v_a_163*pow((((-13+v_c_165+(-1*v_a_163))) / (8)),2)),2)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 2304) << "Wrong result after substitution";
}

TEST(ArithExpr, Test33) {
  int res = v_a_163;
  res -= 14;
  res *= 13;
  res -= 11;
  res += 9;
  res  = pow(res,(v_a_163 % 3));
  res %= v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (pow((-184+(13*v_a_163)),(v_a_163 % (3))) % (v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test34) {
  int res = 7;
  res += v_c_165;
  res += 8;
  res -= v_a_163;
  res += v_b_164;
  res -= v_d_166;
  res *= 8;

  // Partially evaluated expression:
  EXPECT_EQ(res, (120+(-8*v_a_163)+(8*v_b_164)+(8*v_c_165)+(-8*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 464) << "Wrong result after substitution";
}

TEST(ArithExpr, Test35) {
  int res = v_d_166;
  res += 20;
  res -= v_a_163;
  res += 4;
  res *= 20;
  res /= 7;
  res -= 7;

  // Partially evaluated expression:
  EXPECT_EQ(res, (61+(((4+(-20*v_a_163)+(20*v_d_166))) / (7)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 39) << "Wrong result after substitution";
}

TEST(ArithExpr, Test36) {
  int res = 6;
  res += 16;
  res  = pow(res,(7 % 3));
  res *= v_d_166;
  res /= 19;
  res *= v_a_163;
  res -= v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-1*v_a_163)+(v_a_163*(((22*v_d_166)) / (19))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 36) << "Wrong result after substitution";
}

TEST(ArithExpr, Test37) {
  int res = 6;
  res /= 11;
  res *= v_d_166;
  res *= 12;
  res += 1;
  res /= 5;
  res /= 12;

  // Partially evaluated expression:
  EXPECT_EQ(res, 0) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test38) {
  int res = 20;
  res *= 4;
  res *= 17;
  res += 16;
  res -= 16;
  res  = pow(res,(11 % 3));
  res -= 5;

  // Partially evaluated expression:
  EXPECT_EQ(res, 1849595) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1849595) << "Wrong result after substitution";
}

TEST(ArithExpr, Test39) {
  int res = 13;
  res  = pow(res,(7 % 3));
  res  = pow(res,(v_c_165 % 3));
  res %= 10;
  res  = pow(res,(v_d_166 % 3));
  res *= 12;
  res -= 19;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-19+(12*pow((pow(13,(v_c_165 % (3))) % (10)),(v_d_166 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 89) << "Wrong result after substitution";
}

TEST(ArithExpr, Test40) {
  int res = v_d_166;
  res %= v_b_164;
  res -= 19;
  res /= 19;
  res  = pow(res,(20 % 3));
  res  = pow(res,(20 % 3));
  res /= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((pow((-1+(((v_d_166 % (v_b_164))) / (19))),4)) / (v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test41) {
  int res = 18;
  res  = pow(res,(4 % 3));
  res += 10;
  res += 15;
  res *= 10;
  res += v_b_164;
  res += v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (430+(2*v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 544) << "Wrong result after substitution";
}

TEST(ArithExpr, Test42) {
  int res = 11;
  res /= 20;
  res *= 19;
  res %= 13;
  res  = pow(res,(3 % 3));
  res -= v_a_163;
  res += v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (1+v_d_166+(-1*v_a_163))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -7) << "Wrong result after substitution";
}

TEST(ArithExpr, Test43) {
  int res = 3;
  res *= v_b_164;
  res += v_d_166;
  res  = pow(res,(v_c_165 % 3));
  res -= v_b_164;
  res -= 3;
  res  = pow(res,(6 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, 1) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test44) {
  int res = v_a_163;
  res *= 17;
  res %= v_c_165;
  res %= v_d_166;
  res  = pow(res,(v_b_164 % 3));
  res -= 19;
  res *= 13;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-247+(13*pow((((17*v_a_163) % (v_c_165)) % (v_d_166)),(v_b_164 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -234) << "Wrong result after substitution";
}

TEST(ArithExpr, Test45) {
  int res = 9;
  res += 20;
  res %= v_d_166;
  res += 13;
  res %= 19;
  res += 4;
  res -= v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (4+((13+(29 % (v_d_166))) % (19))+(-1*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 14) << "Wrong result after substitution";
}

TEST(ArithExpr, Test46) {
  int res = v_c_165;
  res += v_c_165;
  res -= 18;
  res *= v_a_163;
  res /= 20;
  res += 20;
  res  = pow(res,(14 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((20+((((-9*v_a_163)+(v_a_163*v_c_165))) / (10))),2)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 144) << "Wrong result after substitution";
}

TEST(ArithExpr, Test47) {
  int res = 9;
  res  = pow(res,(7 % 3));
  res += v_d_166;
  res -= 10;
  res -= v_b_164;
  res %= 3;
  res += 3;

  // Partially evaluated expression:
  EXPECT_EQ(res, (3+((-1+v_d_166+(-1*v_b_164)) % (3)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 3) << "Wrong result after substitution";
}

TEST(ArithExpr, Test48) {
  int res = v_c_165;
  res %= 9;
  res  = pow(res,(v_d_166 % 3));
  res += 18;
  res *= v_b_164;
  res += v_c_165;
  res -= 14;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-14+v_c_165+(18*v_b_164)+(v_b_164*pow((v_c_165 % (9)),(v_d_166 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1128) << "Wrong result after substitution";
}

TEST(ArithExpr, Test49) {
  int res = 19;
  res += v_b_164;
  res *= 14;
  res *= 16;
  res /= v_b_164;
  res  = pow(res,(v_c_165 % 3));
  res -= 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-1+pow((224+((4256) / (v_b_164))),(v_c_165 % (3))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 88803) << "Wrong result after substitution";
}

TEST(ArithExpr, Test50) {
  int res = 5;
  res /= v_d_166;
  res += v_a_163;
  res += 7;
  res  = pow(res,(v_c_165 % 3));
  res += v_c_165;
  res  = pow(res,(12 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, 1) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test51) {
  int res = 11;
  res /= 10;
  res  = pow(res,(11 % 3));
  res /= 13;
  res  = pow(res,(v_d_166 % 3));
  res *= 9;
  res *= 3;

  // Partially evaluated expression:
  EXPECT_EQ(res, 0) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test52) {
  int res = 11;
  res *= 6;
  res -= 17;
  res -= v_d_166;
  res -= v_d_166;
  res -= 6;
  res += 7;

  // Partially evaluated expression:
  EXPECT_EQ(res, (50+(-2*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 42) << "Wrong result after substitution";
}

TEST(ArithExpr, Test53) {
  int res = 12;
  res  = pow(res,(v_a_163 % 3));
  res -= 1;
  res += 2;
  res /= 19;
  res *= 11;
  res /= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((11*(((1+pow(12,(v_a_163 % (3))))) / (19)))) / (v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test54) {
  int res = 7;
  res *= 9;
  res += v_d_166;
  res *= 4;
  res /= 5;
  res %= 17;
  res /= 20;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((((50+(((2+(4*v_d_166))) / (5))) % (17))) / (20))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test55) {
  int res = 19;
  res += v_b_164;
  res *= v_c_165;
  res /= v_d_166;
  res += 4;
  res -= v_a_163;
  res *= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-1*v_a_163*v_b_164)+(4*v_b_164)+(v_b_164*((((19*v_c_165)+(v_b_164*v_c_165))) / (v_d_166))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1710) << "Wrong result after substitution";
}

TEST(ArithExpr, Test56) {
  int res = 10;
  res += 6;
  res -= 19;
  res -= 9;
  res += 13;
  res += v_a_163;
  res += 5;

  // Partially evaluated expression:
  EXPECT_EQ(res, (6+v_a_163)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 18) << "Wrong result after substitution";
}

TEST(ArithExpr, Test57) {
  int res = 6;
  res -= 11;
  res += v_a_163;
  res *= 17;
  res /= v_a_163;
  res /= 6;
  res += 14;

  // Partially evaluated expression:
  EXPECT_EQ(res, (16+(((5+((-85) / (v_a_163)))) / (6)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 16) << "Wrong result after substitution";
}

TEST(ArithExpr, Test58) {
  int res = 7;
  res %= 10;
  res += v_b_164;
  res %= 6;
  res %= 9;
  res += 12;
  res  = pow(res,(5 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((12+(((7+v_b_164) % (6)) % (9))),2)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 256) << "Wrong result after substitution";
}

TEST(ArithExpr, Test59) {
  int res = v_d_166;
  res /= v_d_166;
  res -= 7;
  res += v_c_165;
  res *= 2;
  res += 14;
  res += 4;

  // Partially evaluated expression:
  EXPECT_EQ(res, (6+(2*v_c_165))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 10) << "Wrong result after substitution";
}

TEST(ArithExpr, Test60) {
  int res = 13;
  res *= v_a_163;
  res -= v_d_166;
  res /= v_b_164;
  res  = pow(res,(8 % 3));
  res -= 13;
  res *= 16;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-208+(16*pow(((((13*v_a_163)+(-1*v_d_166))) / (v_b_164)),2)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -144) << "Wrong result after substitution";
}

TEST(ArithExpr, Test61) {
  int res = 4;
  res *= 18;
  res -= 1;
  res /= v_a_163;
  res *= v_d_166;
  res -= 1;
  res *= v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-1*v_d_166)+(((71) / (v_a_163))*pow(v_d_166,2)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 76) << "Wrong result after substitution";
}

TEST(ArithExpr, Test62) {
  int res = 7;
  res += v_a_163;
  res  = pow(res,(v_c_165 % 3));
  res /= v_b_164;
  res -= 10;
  res += 10;
  res -= 6;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-6+((pow((7+v_a_163),(v_c_165 % (3)))) / (v_b_164)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test63) {
  int res = 18;
  res %= v_d_166;
  res -= 20;
  res  = pow(res,(v_a_163 % 3));
  res += 14;
  res -= 18;
  res /= v_c_165;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((-4+pow((-20+(18 % (v_d_166))),(v_a_163 % (3))))) / (v_c_165))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test64) {
  int res = 12;
  res *= 20;
  res *= v_b_164;
  res -= 17;
  res += 3;
  res %= 16;
  res += v_c_165;

  // Partially evaluated expression:
  EXPECT_EQ(res, (v_c_165+((-14+(240*v_b_164)) % (16)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 4) << "Wrong result after substitution";
}

TEST(ArithExpr, Test65) {
  int res = 4;
  res %= v_c_165;
  res %= 19;
  res /= 4;
  res -= 11;
  res *= 8;
  res /= 3;

  // Partially evaluated expression:
  EXPECT_EQ(res, 0) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test66) {
  int res = v_c_165;
  res *= 14;
  res -= 10;
  res  = pow(res,(v_d_166 % 3));
  res  = pow(res,(10 % 3));
  res *= 15;
  res -= 9;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-9+(15*pow((-10+(14*v_c_165)),(v_d_166 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 261) << "Wrong result after substitution";
}

TEST(ArithExpr, Test67) {
  int res = 3;
  res *= 12;
  res += v_c_165;
  res *= v_c_165;
  res += 1;
  res *= 10;
  res /= 10;

  // Partially evaluated expression:
  EXPECT_EQ(res, (1+(36*v_c_165)+pow(v_c_165,2))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 77) << "Wrong result after substitution";
}

TEST(ArithExpr, Test68) {
  int res = 12;
  res += 16;
  res /= v_c_165;
  res *= 1;
  res += 11;
  res *= v_a_163;
  res  = pow(res,(1 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, ((11*v_a_163)+(v_a_163*((28) / (v_c_165))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 300) << "Wrong result after substitution";
}

TEST(ArithExpr, Test69) {
  int res = 17;
  res *= 3;
  res -= 18;
  res *= v_b_164;
  res *= 13;
  res += 7;
  res -= 18;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-11+(429*v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 24442) << "Wrong result after substitution";
}

TEST(ArithExpr, Test70) {
  int res = 2;
  res += v_a_163;
  res *= 2;
  res %= v_d_166;
  res -= 7;
  res -= 11;
  res /= 20;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((-18+((4+(2*v_a_163)) % (v_d_166)))) / (20))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test71) {
  int res = 19;
  res *= 19;
  res += 14;
  res *= 2;
  res += v_a_163;
  res += v_b_164;
  res -= 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (749+v_a_163+v_b_164)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 818) << "Wrong result after substitution";
}

TEST(ArithExpr, Test72) {
  int res = v_b_164;
  res += 16;
  res  = pow(res,(2 % 3));
  res -= 10;
  res %= v_c_165;
  res *= 17;
  res *= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (17*v_b_164*((-10+pow((16+v_b_164),2)) % (v_c_165)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 969) << "Wrong result after substitution";
}

TEST(ArithExpr, Test73) {
  int res = 17;
  res /= v_b_164;
  res -= v_b_164;
  res += 11;
  res *= 8;
  res += v_a_163;
  res /= v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, (1+(((88+(-8*v_b_164)+(8*((17) / (v_b_164))))) / (v_a_163)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -29) << "Wrong result after substitution";
}

TEST(ArithExpr, Test74) {
  int res = v_a_163;
  res -= 15;
  res += 8;
  res /= 5;
  res  = pow(res,(v_d_166 % 3));
  res %= v_b_164;
  res  = pow(res,(11 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((pow((((-7+v_a_163)) / (5)),(v_d_166 % (3))) % (v_b_164)),2)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1) << "Wrong result after substitution";
}

TEST(ArithExpr, Test75) {
  int res = 2;
  res += v_a_163;
  res += 6;
  res -= v_d_166;
  res *= v_a_163;
  res /= v_b_164;
  res  = pow(res,(20 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow(((((-1*v_a_163*v_d_166)+(8*v_a_163)+pow(v_a_163,2))) / (v_b_164)),2)) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 9) << "Wrong result after substitution";
}

TEST(ArithExpr, Test76) {
  int res = v_c_165;
  res %= v_b_164;
  res += 10;
  res /= 3;
  res %= 14;
  res += 19;
  res  = pow(res,(v_c_165 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, pow((19+((3+(((1+(v_c_165 % (v_b_164)))) / (3))) % (14))),(v_c_165 % (3)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 529) << "Wrong result after substitution";
}

TEST(ArithExpr, Test77) {
  int res = 18;
  res *= 4;
  res %= 18;
  res += 13;
  res  = pow(res,(15 % 3));
  res *= 1;
  res -= 16;

  // Partially evaluated expression:
  EXPECT_EQ(res, -15) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -15) << "Wrong result after substitution";
}

TEST(ArithExpr, Test78) {
  int res = 17;
  res *= 3;
  res += 3;
  res += 18;
  res  = pow(res,(v_b_164 % 3));
  res += v_a_163;
  res /= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((v_a_163+pow(72,(v_b_164 % (3))))) / (v_b_164))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test79) {
  int res = 9;
  res *= v_a_163;
  res -= v_a_163;
  res *= v_b_164;
  res  = pow(res,(v_d_166 % 3));
  res -= 15;
  res -= 8;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-23+pow((8*v_a_163*v_b_164),(v_d_166 % (3))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 5449) << "Wrong result after substitution";
}

TEST(ArithExpr, Test80) {
  int res = 12;
  res  = pow(res,(17 % 3));
  res -= 3;
  res %= 2;
  res /= v_a_163;
  res += v_b_164;
  res += 9;

  // Partially evaluated expression:
  EXPECT_EQ(res, (9+v_b_164+((1) / (v_a_163)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 66) << "Wrong result after substitution";
}

TEST(ArithExpr, Test81) {
  int res = 19;
  res *= 11;
  res += 18;
  res *= v_a_163;
  res -= 20;
  res %= 16;
  res  = pow(res,(7 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-20+(227*v_a_163)) % (16))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test82) {
  int res = 16;
  res += 12;
  res += 20;
  res %= v_b_164;
  res -= v_a_163;
  res -= 7;
  res *= v_c_165;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-7*v_c_165)+(-1*v_a_163*v_c_165)+(v_c_165*(48 % (v_b_164))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 58) << "Wrong result after substitution";
}

TEST(ArithExpr, Test83) {
  int res = 12;
  res += v_d_166;
  res -= 8;
  res -= v_a_163;
  res %= 1;
  res *= v_b_164;
  res *= v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, 0) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test84) {
  int res = 14;
  res -= 7;
  res  = pow(res,(8 % 3));
  res  = pow(res,(3 % 3));
  res += 17;
  res %= v_a_163;
  res  = pow(res,(7 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, (18 % (v_a_163))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 6) << "Wrong result after substitution";
}

TEST(ArithExpr, Test85) {
  int res = 12;
  res  = pow(res,(2 % 3));
  res -= v_c_165;
  res  = pow(res,(17 % 3));
  res += v_c_165;
  res -= 10;
  res += v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (-10+v_c_165+v_d_166+pow((144+(-1*v_c_165)),2))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 20160) << "Wrong result after substitution";
}

TEST(ArithExpr, Test86) {
  int res = 2;
  res -= 2;
  res += 10;
  res *= 18;
  res += 8;
  res %= v_d_166;
  res *= v_a_163;

  // Partially evaluated expression:
  EXPECT_EQ(res, (v_a_163*(188 % (v_d_166)))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test87) {
  int res = v_c_165;
  res += v_a_163;
  res *= 8;
  res *= v_b_164;
  res %= 1;
  res -= 5;
  res -= 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, -6) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -6) << "Wrong result after substitution";
}

TEST(ArithExpr, Test88) {
  int res = 10;
  res *= v_b_164;
  res += v_d_166;
  res -= 11;
  res *= v_c_165;
  res += v_a_163;
  res += v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, (v_a_163+v_d_166+(-11*v_c_165)+(10*v_b_164*v_c_165)+(v_c_165*v_d_166))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 1142) << "Wrong result after substitution";
}

TEST(ArithExpr, Test89) {
  int res = 13;
  res  = pow(res,(2 % 3));
  res -= v_b_164;
  res *= v_a_163;
  res /= 9;
  res /= 15;
  res %= 16;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((((-1*v_a_163*v_b_164)+(169*v_a_163))) / (135)) % (16))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 9) << "Wrong result after substitution";
}

TEST(ArithExpr, Test90) {
  int res = 2;
  res %= 3;
  res %= 16;
  res /= v_d_166;
  res *= 15;
  res += v_c_165;
  res += 13;

  // Partially evaluated expression:
  EXPECT_EQ(res, (13+v_c_165+(15*((2) / (v_d_166))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 15) << "Wrong result after substitution";
}

TEST(ArithExpr, Test91) {
  int res = 15;
  res /= v_b_164;
  res *= v_d_166;
  res += 15;
  res += 19;
  res -= v_a_163;
  res %= 8;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((34+(-1*v_a_163)+(v_d_166*((15) / (v_b_164)))) % (8))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 6) << "Wrong result after substitution";
}

TEST(ArithExpr, Test92) {
  int res = v_d_166;
  res  = pow(res,(v_b_164 % 3));
  res  = pow(res,(12 % 3));
  res *= 18;
  res  = pow(res,(3 % 3));
  res -= v_a_163;
  res += 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (2+(-1*v_a_163))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -10) << "Wrong result after substitution";
}

TEST(ArithExpr, Test93) {
  int res = v_a_163;
  res -= v_a_163;
  res += 9;
  res += v_c_165;
  res -= v_c_165;
  res *= v_a_163;
  res += 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (1+(9*v_a_163))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 109) << "Wrong result after substitution";
}

TEST(ArithExpr, Test94) {
  int res = 10;
  res -= v_b_164;
  res  = pow(res,(v_c_165 % 3));
  res += 9;
  res -= v_c_165;
  res *= 4;
  res  = pow(res,(13 % 3));

  // Partially evaluated expression:
  EXPECT_EQ(res, (36+(-4*v_c_165)+(4*pow((10+(-1*v_b_164)),(v_c_165 % (3)))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 8864) << "Wrong result after substitution";
}

TEST(ArithExpr, Test95) {
  int res = v_a_163;
  res -= 18;
  res -= 3;
  res -= 12;
  res /= 11;
  res -= 20;
  res *= v_d_166;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((-23*v_d_166)+(v_d_166*((v_a_163) / (11))))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, -88) << "Wrong result after substitution";
}

TEST(ArithExpr, Test96) {
  int res = 4;
  res /= 17;
  res += 13;
  res /= 13;
  res *= 15;
  res  = pow(res,(6 % 3));
  res *= v_b_164;

  // Partially evaluated expression:
  EXPECT_EQ(res, v_b_164) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 57) << "Wrong result after substitution";
}

TEST(ArithExpr, Test97) {
  int res = 5;
  res /= 19;
  res %= 2;
  res += v_a_163;
  res += 2;
  res /= 19;
  res /= 19;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((2+v_a_163)) / (361))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test98) {
  int res = 18;
  res *= 1;
  res -= 12;
  res *= 15;
  res *= v_b_164;
  res -= v_c_165;
  res -= v_c_165;

  // Partially evaluated expression:
  EXPECT_EQ(res, ((90*v_b_164)+(-2*v_c_165))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 5126) << "Wrong result after substitution";
}

TEST(ArithExpr, Test99) {
  int res = v_b_164;
  res += 15;
  res /= 6;
  res /= 10;
  res *= v_d_166;
  res /= 11;
  res *= 1;

  // Partially evaluated expression:
  EXPECT_EQ(res, (((v_d_166*(((2+(((3+v_b_164)) / (6)))) / (10)))) / (11))) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}

TEST(ArithExpr, Test100) {
  int res = 7;
  res -= 2;
  res %= 7;
  res -= 3;
  res -= 9;
  res += 5;
  res /= 15;

  // Partially evaluated expression:
  EXPECT_EQ(res, 0) << "Partially evaluated expression does not match";

  // After substitution:
  EXPECT_EQ(res, 0) << "Wrong result after substitution";
}



int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
      