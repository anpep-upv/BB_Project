with BB, BB.ADC;
use BB, BB.ADC;

package ADC_Utils is
   function ADC_To_Position (DR_Value : ADC_Register) return Float;
   function Get_CR
     (Trigger : Boolean := False; Interrupt_Enable : Boolean := False)
      return ADC_Register;
   function Is_Conversion_Complete(DR_Value : ADC_Register) return Boolean;
end ADC_Utils;
