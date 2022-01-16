with BB, BB.ADC;
use BB, BB.ADC;

package body ADC_Utils is
   -----------------------
   --  ADC_To_Position  --
   -----------------------

   function ADC_To_Position (DR_Value : ADC_Register) return Float is
      -- Obtain first 12 bits
      ADC_Count : ADC_Register := DR_Value and (2**12 - 1);
   begin
      return Min_Position +
        Float (ADC_Count) * ((Max_Position - Min_Position) / 4095.0);
   end ADC_To_Position;

   --------------
   --  Get_CR  --
   --------------

   function Get_CR
     (Trigger : Boolean := False; Interrupt_Enable : Boolean := False)
      return ADC_Register
   is
      Flag_TRG : ADC_Register := 2**0;  -- Bit 0
      Flag_IE  : ADC_Register := 2**2;  -- Bit 2

      CR_Value : ADC_Register := 0;
   begin
      if Trigger then
         CR_Value := CR_Value or Flag_TRG;
      end if;

      if Interrupt_Enable then
         CR_Value := CR_Value or Flag_IE;
      end if;

      return CR_Value;
   end Get_CR;

   ------------------------------
   --  Is_Conversion_Complete  --
   ------------------------------

   function Is_Conversion_Complete (DR_Value : ADC_Register) return Boolean is
      Flag_EOC : ADC_Register := 2**15; -- Bit 15
   begin
      return (DR_Value and Flag_EOC) = Flag_EOC;
   end Is_Conversion_Complete;
end ADC_Utils;
