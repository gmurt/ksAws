program ListBuckets;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {Form10},
  ksAwsS3 in '..\..\ksAwsS3.pas',
  ksAwsBase in '..\..\ksAwsBase.pas',
  ksAwsConst in '..\..\ksAwsConst.pas',
  ksAwsHash in '..\..\ksAwsHash.pas',
  ksAwsSes in '..\..\ksAwsSes.pas',
  ksAwsHttpIntf in '..\..\ksAwsHttpIntf.pas',
  ksAwsHttpNetClient in '..\..\ksAwsHttpNetClient.pas',
  ksAwsHttpIndy in '..\..\ksAwsHttpIndy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
