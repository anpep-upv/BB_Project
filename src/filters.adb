with BB;          use BB;
with BB.ADC;      use BB.ADC;
with ADC_Handler; use ADC_Handler;
with ADC_Utils;   use ADC_Utils;
with CSV_Logs;    use CSV_Logs;
with IIR_Filters; use IIR_Filters;

with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

---------------
--  Filters  --
---------------

procedure Filters is
   ADC_Count : ADC_Register;
begin
   Open_Log_Session ("filters.csv");
   Log_Text ("Raw_Pos, IIR-80/20, IIR-50/50");

   -- Register interrupt handler
   Attach_ADC_Handler (EOC_Handler'Access);

   for I in 1 .. 100 loop
      -- Trigger conversion with interrupts enabled
      Set_False (End_Of_Conversion);
      Write_CR (Get_CR (Trigger => True, Interrupt_Enable => True));

      -- Wait for EOC Suspension Object
      Suspend_Until_True (End_Of_Conversion);

      -- Obtain first 12 MSBs
      ADC_Count := Read_DR and (2**12 - 1);

      -- Write the conversion result
      Log_Data
        ((ADC_To_Position (ADC_Count), Filter_80 (ADC_Count),
          Filter_50 (ADC_Count)));
   end loop;

   Close_Log_Session;
end Filters;
