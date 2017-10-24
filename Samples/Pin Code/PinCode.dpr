program PinCode;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in '..\Pin Code - Copy\untMain.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
