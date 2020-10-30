unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm10 = class(TForm)
    lbInstances: TListBox;
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

uses ksAwsEc2, ksAwsBase;

// replace the values inside "credentials.inc" with your AWS credentials which you can
// create in the AWS IAM Console.

{$INCLUDE _credentials.inc}

const
  C_AWS_RGN = awsEuWest1;

{$R *.dfm}

procedure TForm10.btnGetVerifiedSendersClick(Sender: TObject);
var
  AEc2: IksAwsEC2;
  AInstances: TksAwsEC2InstanceList;
  AInstance: IksAwsEC2Instance;
begin
  lbInstances.Items.Create;
  if Pos('*', C_EC2_ACCESS_KEY) > 0 then
  begin
    ShowMessage('Please replace the C_EC2_ACCESS_KEY and C_EC2_SECRET_KEY const in the credentials.inc file.');
    Exit;
  end;
  AInstances := TksAwsEC2InstanceList.Create;
  try
    AEc2 := CreateAwsEc2(C_EC2_ACCESS_KEY, C_EC2_SECRET_KEY, C_AWS_RGN);
    AEc2.ListInstances(AInstances);
    for AInstance in AInstances do
      lbInstances.Items.Add(AInstance.ID+' - '+AInstance.Name+' - '+ AInstance.Status);
  finally
    AInstances.Free;
  end;
end;

end.
