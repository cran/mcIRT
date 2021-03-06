#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::NumericVector de1nelmC(List PITEMLL, List QUADS, List fiqG, List riqv_querG, List fique0G) {

      int ngru = PITEMLL.size(); // number of groups
    
      //// ich muss hier wissen wieviele gruppen * items * catjeitem ich habe wegen des response vectors
    NumericMatrix meas = riqv_querG[0];
    int iticatg =  meas.nrow() * ngru * 2;
    
    NumericVector derivsG(iticatg); // the endvector
    int indexdrivs = 0; // index for the endvector
    
    //Rcout << "The value is " << iticatg << std::endl;
    
    for(int gru =0; gru < ngru; gru++) // loops the groups
    
    {
    
    List quads = QUADS[gru];
    NumericVector nodes = quads[0];
    NumericMatrix FIQ = fiqG[gru];
    NumericMatrix FIQ0 = fique0G[gru];
    NumericMatrix RIQ = riqv_querG[gru];
    List PITEML = PITEMLL[gru];
    

    int lno = nodes.size(); //number of nodes

    int itzahl = PITEML.size(); // number of items
    int gescat = 0; 
    
  


    //NumericVector derivs(gescat*2);

    int endE =0;

    
    for(int its = 0; its < itzahl; its++)
      { // loops items
        
      Rcpp::NumericVector PITEM = PITEML[its]; // extracts pars for each item
      
      
      int lpi = PITEM.size()/2; // number of categories
      
       Rcpp::NumericMatrix x(lno,lpi);
       
       
        for(int o = 0; o < lno; o++)
       {
        double gessum = 0;
        double z2plv = 0;
        double tplf = 0;
        
       for(int q = 0; q < lpi; q++)
         {
         int lpi2 = q+lpi;
         if(q == 0)
         {
          z2plv = exp(PITEM(q) + nodes(o)*PITEM(lpi2)) / ( 1+ exp(PITEM(q) + nodes(o)*PITEM(lpi2)));
          tplf = 1 - z2plv; // 1-P

         } else {
                x(o,q) = exp(PITEM(q) + nodes(o)*PITEM(lpi2)); // different than Enlm
                gessum += x(o,q);
                }
         }
        x(o,_) = x(o,_) / gessum;
        x(o,0) = z2plv;
        
        //std::cout << "firstofNRMpart: " << x(o,1) << " \n "; //deleteme
        //std::cout << "z2plv:" << z2plv << " \n ";
        // std::cout << "inmattrix" << x(o,0) << " \n ";
       }
       
       
// old nrm thing       
//       for(int o = 0; o < lno; o++)
//         { //loops the nodes
//         double gessum = 0;
//           
//           for(int q = 0; q < lpi; q++)
//             { //loops categories
//             int lpi2 = q+lpi;
//             x(o,q) = exp(PITEM(q) + nodes(o)*PITEM(lpi2));
//             gessum += x(o,q);
//             }
//
//             x(o,_) =  x(o,_) /gessum;
//             
//         }

        
       for(int blab = 0; blab < lpi; blab++)
           { //loops categories
           
           if(blab == 0)
             { // 2pl part - it differs only in the FIQs
             derivsG(indexdrivs) = sum(RIQ(endE,_) - x(_,blab) * FIQ(_,its));
             int woxi = indexdrivs + lpi; // position of the xi
             derivsG(woxi) = sum((RIQ(endE,_) - x(_,blab) * FIQ(_,its)) * nodes);
             endE += 1;
             indexdrivs += 1;
             } else { //nrm part
                     derivsG(indexdrivs) = sum(RIQ(endE,_) - x(_,blab) * FIQ0(_,its));
                     int woxi = indexdrivs + lpi; // position of the xi
                     derivsG(woxi) = sum((RIQ(endE,_) - x(_,blab) * FIQ0(_,its)) * nodes);
                     endE += 1;
                     indexdrivs += 1;
                     }
           
           }

//      Rcout << "The value is " << endE << std::endl;
//      Rcout << "The value is " << derivs.size() << std::endl;

        indexdrivs = indexdrivs + lpi;

      }
        
      
      
    }
    
    
return derivsG;

}

