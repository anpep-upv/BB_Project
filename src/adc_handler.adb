package body ADC_Handler is

   -----------------
   -- EOC_Handler --
   -----------------

   procedure EOC_Handler is
   begin
      Set_True (End_Of_Conversion);
   end EOC_Handler;

end ADC_Handler;
