#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double phi(int i, int j, NumericVector comparator, NumericVector this_endpoint) {
  if (i == j) return 0;
  if ((comparator[i] > comparator[j] && this_endpoint[i] > this_endpoint[j]) ||
      (comparator[j] > comparator[i] && this_endpoint[j] > this_endpoint[i])) return 1;
  if (comparator[i] == comparator[j] || this_endpoint[i] == this_endpoint[j]) return 0.5;
  return 0;
}

// [[Rcpp::export]]
NumericVector calculate_sum_phi(NumericVector comparator, NumericVector this_endpoint, int n) {
  NumericVector sum_phi(n * n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      sum_phi[i * n + j] = phi(i, j, comparator, this_endpoint);
    }
  }
  return sum_phi;
}

// [[Rcpp::export]]
NumericVector calculate_struct_components(NumericVector comparator, NumericVector this_endpoint, int n) {
  NumericVector struct_components(n);
  for (int i = 0; i < n; ++i) {
    double sum = 0;
    for (int j = 0; j < n; ++j) {
      if (i != j) {
        sum += phi(i, j, comparator, this_endpoint);
      }
    }
    struct_components[i] = sum / (n - 1);
  }
  return struct_components;
}