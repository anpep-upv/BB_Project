with BB;          use BB;
with BB.ADC;      use BB.ADC;
with ADC_Handler; use ADC_Handler;
with ADC_Utils;   use ADC_Utils;
with CSV_Logs;    use CSV_Logs;

with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

---------------
--  Filters  --
---------------

procedure Filters is
   ADC_Count : ADC_Register;

   Historic_Sample_80 : Integer := 0;  -- Historic mean sample for IIR-80/20
   IIR_80_N           : Integer := 0;  -- Number of samples for IIR-80/20
   
   -----------------
   --  Filter_80  --
   -----------------

   function Filter_80 (ADC_Sample : in ADC_Register) return Position is
   begin
      -- Increment N and add this sample to the historic sample
      Historic_Sample_80 := Integer(ADC_Sample) + Historic_Sample_80;
      IIR_80_N           := 1 + IIR_80_N;

      -- Perform conversion to mm and apply weighted IIR filter
      return ADC_To_Position(ADC_Register(0.8 * (Float(Historic_Sample_80) / Float(IIR_80_N)) + 0.2 * Float(ADC_Sample)));
   end Filter_80;

   Historic_Sample_50 : Integer := 0;  -- Historic mean sample for IIR-50/50
   IIR_50_N           : Integer := 0;  -- Number of samples for IIR-50/50
   
   -----------------
   --  Filter_50  --
   -----------------

   function Filter_50 (ADC_Sample : in ADC_Register) return Position is
   begin
      -- Increment N and add this sample to the historic sample
      Historic_Sample_50 := Integer(ADC_Sample) + Historic_Sample_50;
      IIR_50_N           := 1 + IIR_50_N;

      -- Perform conversion to mm and apply weighted IIR filter
      return ADC_To_Position(ADC_Register(0.5 * (Float(Historic_Sample_80) / Float(IIR_80_N)) + 0.5 * Float(ADC_Sample)));
   end Filter_50;

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
