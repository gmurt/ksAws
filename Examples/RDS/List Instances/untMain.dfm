object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 325
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbInstances: TListBox
    Left = 0
    Top = 49
    Width = 339
    Height = 276
    Style = lbOwnerDrawFixed
    Align = alClient
    ItemHeight = 21
    TabOrder = 0
  end
  object btnGetVerifiedSenders: TButton
    Left = 0
    Top = 0
    Width = 339
    Height = 49
    Align = alTop
    Caption = 'List Instances'
    TabOrder = 1
    OnClick = btnGetVerifiedSendersClick
  end
end
