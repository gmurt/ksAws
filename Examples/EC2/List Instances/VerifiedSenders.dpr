program VerifiedSenders;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {Form10};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
