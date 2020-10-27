object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 306
  ClientWidth = 286
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
    Width = 286
    Height = 257
    Style = lbOwnerDrawFixed
    Align = alClient
    ItemHeight = 21
    TabOrder = 0
    ExplicitLeft = -8
    ExplicitTop = 55
  end
  object btnGetVerifiedSenders: TButton
    Left = 0
    Top = 0
    Width = 286
    Height = 49
    Align = alTop
    Caption = 'List Instances'
    TabOrder = 1
    OnClick = btnGetVerifiedSendersClick
    ExplicitWidth = 250
  end
end
