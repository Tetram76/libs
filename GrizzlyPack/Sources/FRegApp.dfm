object FenRegApp: TFenRegApp
  Left = 144
  Top = 82
  ActiveControl = EditCustomerName
  BorderStyle = bsDialog
  Caption = 'Enregistrement de '
  ClientHeight = 218
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 237
    Height = 177
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 70
    Height = 14
    Caption = 'Raison sociale'
  end
  object Label2: TLabel
    Left = 12
    Top = 52
    Width = 63
    Height = 14
    Caption = 'N° de licence'
  end
  object Label3: TLabel
    Left = 144
    Top = 52
    Width = 74
    Height = 14
    Caption = 'Clé personnelle'
  end
  object Label4: TLabel
    Left = 12
    Top = 136
    Width = 108
    Height = 14
    Caption = 'Code d'#39'enregistrement'
  end
  object LabelMasterNumber: TLabel
    Left = 12
    Top = 92
    Width = 95
    Height = 14
    Caption = 'N° de licence maître'
    Enabled = False
  end
  object btnOk: TBitBtn
    Left = 44
    Top = 188
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 126
    Top = 188
    Width = 75
    Height = 25
    TabOrder = 6
    Kind = bkCancel
  end
  object EditCustomerName: TEdit
    Left = 12
    Top = 28
    Width = 221
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = EditCustomerNameChange
  end
  object EditReleaseKey: TEdit
    Left = 144
    Top = 68
    Width = 89
    Height = 22
    TabStop = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object EditLicenseNumber: TMaskEdit
    Left = 12
    Top = 68
    Width = 121
    Height = 22
    EditMask = '0000-0000;1;_'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    TabOrder = 1
    Text = '    -    '
    OnChange = EditLicenseNumberChange
  end
  object EditRegistrationCode: TMaskEdit
    Left = 12
    Top = 152
    Width = 221
    Height = 22
    EditMask = 'AAAA-AAAA-AAAA;1;_'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    MaxLength = 14
    ParentFont = False
    TabOrder = 4
    Text = '    -    -    '
    OnChange = EditRegistrationCodeChange
  end
  object EditMasterNumber: TMaskEdit
    Left = 12
    Top = 108
    Width = 221
    Height = 22
    Enabled = False
    EditMask = '0000-0000;1;_'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    TabOrder = 3
    Text = '    -    '
    OnChange = EditMasterNumberChange
  end
end
