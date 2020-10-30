unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    lbInstances: TListBox;
    btnGetVerifiedSenders: TButton;
    procedure btnGetVerifiedSendersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ksAwsRDS, ksAwsBase;

{$I credentials.inc}

{$R *.dfm}

procedure TForm1.btnGetVerifiedSendersClick(Sender: TObject);
var
  AEc2: IksAwsRDS;
  AInstances: TksAwsRDSInstanceList;
  AInstance: IksAwsRDSInstance;
begin
  lbInstances.Items.Create;
  if Pos('*', C_RDS_ACCESS_KEY) > 0 then
  begin
    ShowMessage('Please replace the C_RDS_ACCESS_KEY and C_RDS_SECRET_KEY const in the credentials.inc file.');
    Exit;
  end;
  AInstances := TksAwsRDSInstanceList.Create;
  try
    AEc2 := CreateAwsRDS(C_RDS_ACCESS_KEY, C_RDS_SECRET_KEY, awsEuWest1);
    AEc2.ListInstances(AInstances);
    for AInstance in AInstances do
      lbInstances.Items.Add(AInstance.Name+' - '+ AInstance.Status);
  finally
    AInstances.Free;
  end;
end;

end.
