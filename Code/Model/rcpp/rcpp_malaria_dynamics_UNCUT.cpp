#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]


List Erlang_Malaria(
    double t,  
    NumericVector y, 
    NumericVector params) {
  
  double lambda =params["lambda"];
  double  K =    params["K"];
  double  pmax =  params["pmax"]; 
  double  muR =   params["muR"];
  double  muI =   params["muI"];
  double  c =     params["c"];
  double  B =     params["B"]; 
  double  alpha1 = params["alpha1"]; 
  double  alpha2 = params["alpha2"];
  double  muM = params["muM"]; 
  double  muG = params["muG"];
  int    n1 =     params["n1"];
  int    n2 =     params["n2"];
  
  Rcpp::NumericVector dy(n1+n2+3);
  
  double p = pmax;
  
  ///////////////////
  //Red Blood Cells//
  ///////////////////
  
  dy[0] = lambda*(1-((y[0])/K)) - (p*y[0]*y[n1+1]) - muR * y[0];
  
  ////////////////////////////
  //Infected Red Blood Cells//
  ////////////////////////////
  
  dy[1] = ((1.0 - c)* p * y[0] * y[n1+1]) - (n1 * alpha1 * y[1])- (muI * y[1]);
  
  for (int i=1; i < n1; ++i) {
    dy[1+i] = (n1 * alpha1 * y[i]) - (n1 * alpha1 * y[i+1])- (muI  * y[1+i]);  
  }
  
  //////////////
  //Merozoites//
  //////////////
  
  dy[n1+1]  =  B*(n1*alpha1*y[n1]) - (p * y[n1+1]* y[0]) - muM * y[n1+1]; 
  
  ////////////////////////
  //immature gametocytes//
  ////////////////////////
  
  dy[n1+2] =  (c * p * y[0] * y[n1+1]) - muR * y[n1+2] - alpha2*n2*y[n1+2];
  
  for (int i=1; i < n2; ++i) {
    dy[n1+2+i] = alpha2*n2*y[n1+1+i] - n2*alpha2*y[n1+2+i] - muR*y[n1+2+i];
  }
  
  //gametocytes
  dy[n1+n2+2] = (n2 * alpha2 * y[n1+n2+1]) - muG*y[n1+n2+2] ;
  
  
  return List ::create(dy);
  
}