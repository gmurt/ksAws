program VerifiedSenders;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {Form10},
  ksAwsSes in '..\..\ksAwsSes.pas',
  ksAwsBase in '..\..\ksAwsBase.pas',
  ksAwsConst in '..\..\ksAwsConst.pas',
  ksAwsHash in '..\..\ksAwsHash.pas',
  ksAwsHttpIntf in '..\..\ksAwsHttpIntf.pas',
  ksAwsHttpNetClient in '..\..\ksAwsHttpNetClient.pas',
  ksAwsHttpIndy in '..\..\ksAwsHttpIndy.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
