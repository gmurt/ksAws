unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm10 = class(TForm)
    lbSenders: TListBox;
    btnGetVerifiedSenders: TButton;
    procedure btnGetVerifiedSendersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses ksAwsSes, ksAwsBase;

// replace the values inside "credentials.inc" with your SES SMTP credentials which you can
// create in the SES->SMTP Settings section of the AWS Console.

{$INCLUDE credentials.inc}

const
  C_AWS_RGN = awsEuWest1;

{$R *.dfm}

procedure TForm10.btnGetVerifiedSendersClick(Sender: TObject);
var
  ASes: IksAwsSES;
begin
  if Pos('*', C_SES_PUBLIC) > 0 then
  begin
    ShowMessage('Please replace the C_SES_PUBLIC and C_SES_PRIVATE const in untMain.pas with your SES SMTP credentials.');
    Exit;
  end;
  ASes := CreateSes(C_SES_PUBLIC, C_SES_PRIVATE, C_AWS_RGN);
  ASes.GetSenders(lbSenders.Items, 20);
end;


end.
