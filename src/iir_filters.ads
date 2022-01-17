with BB;     use BB;
with BB.ADC; use BB.ADC;

package IIR_Filters is
   function Filter_80 (ADC_Sample : in ADC_Register) return Position;
   function Filter_50 (ADC_Sample : in ADC_Register) return Position;
end IIR_Filters;
