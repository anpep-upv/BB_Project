with BB;        use BB;
with BB.ADC;    use BB.ADC;
with ADC_Utils; use ADC_Utils;

package body IIR_Filters is

   Historic_Sample_80 : Integer := 0;  -- Historic mean sample for IIR-80/20
   IIR_80_N           : Integer := 0;  -- Number of samples for IIR-80/20

   -----------------
   --  Filter_80  --
   -----------------

   function Filter_80 (ADC_Sample : in ADC_Register) return Position is
   begin
      -- Increment N and add this sample to the historic sample
      Historic_Sample_80 := Integer (ADC_Sample) + Historic_Sample_80;
      IIR_80_N           := 1 + IIR_80_N;

      -- Perform conversion to mm and apply weighted IIR filter
      return ADC_To_Position
          (ADC_Register
             (0.8 * (Float (Historic_Sample_80) / Float (IIR_80_N)) +
              0.2 * Float (ADC_Sample)));
   end Filter_80;

   Historic_Sample_50 : Integer := 0;  -- Historic mean sample for IIR-50/50
   IIR_50_N           : Integer := 0;  -- Number of samples for IIR-50/50

   -----------------
   --  Filter_50  --
   -----------------

   function Filter_50 (ADC_Sample : in ADC_Register) return Position is
   begin
      -- Increment N and add this sample to the historic sample
      Historic_Sample_50 := Integer (ADC_Sample) + Historic_Sample_50;
      IIR_50_N           := 1 + IIR_50_N;

      -- Perform conversion to mm and apply weighted IIR filter
      return ADC_To_Position
          (ADC_Register
             (0.5 * (Float (Historic_Sample_80) / Float (IIR_80_N)) +
              0.5 * Float (ADC_Sample)));
   end Filter_50;

   --------------------
   --  Filter_Dummy  --
   --------------------

   function Filter_Dummy (ADC_Sample : in ADC_Register) return Position is
   begin
      return ADC_To_Position(ADC_Sample);
   end Filter_Dummy;
end IIR_Filters;
