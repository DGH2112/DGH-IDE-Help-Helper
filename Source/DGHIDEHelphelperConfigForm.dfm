object frmDGHIDEHelphelperConfig: TfrmDGHIDEHelphelperConfig
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'DGH IDE Help Helper Configuration'
  ClientHeight = 362
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  DesignSize = (
    584
    362)
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 420
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 501
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object pnlFrame: TPanel
    Left = 8
    Top = 8
    Width = 568
    Height = 315
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
  end
end
