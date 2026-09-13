int gcd(int a, int b) {
  if (a==0) {
    return b;
  }
  if (b==0)
    return a;
  if (b<a)
    return gcd(a-b, b);
  return gcd(a, b-a);
}