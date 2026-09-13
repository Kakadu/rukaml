int m91(int n) {
  if (100 < n) {
    return n-10;
  }
  return m91(m91(n + 11));
}
