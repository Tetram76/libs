object FenEditElements: TFenEditElements
  Left = 277
  Top = 87
  BorderStyle = bsDialog
  Caption = 'Gestion des droits d'#39'acc'#232's '
  ClientHeight = 404
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 364
    Align = alClient
    BorderWidth = 3
    Caption = 'Panel1'
    TabOrder = 0
    object PanelPages: TPanel
      Left = 4
      Top = 4
      Width = 574
      Height = 137
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      object Bevel4: TBevel
        Left = 420
        Top = 4
        Width = 149
        Height = 129
      end
      object Bevel2: TBevel
        Left = 4
        Top = 4
        Width = 245
        Height = 37
      end
      object Bevel1: TBevel
        Left = 4
        Top = 44
        Width = 245
        Height = 89
      end
      object LabelElemName: TLabel
        Left = 24
        Top = 16
        Width = 76
        Height = 13
        Alignment = taRightJustify
        Caption = 'Nom '#233'l'#233'ment'
      end
      object LabelReqAccessRight: TLabel
        Left = 12
        Top = 56
        Width = 124
        Height = 13
        Alignment = taRightJustify
        Caption = 'Niveau d'#39'acc'#232's requis'
      end
      object LabelReqPassword: TLabel
        Left = 22
        Top = 80
        Width = 114
        Height = 13
        Alignment = taRightJustify
        Caption = 'Mot de passe requis'
      end
      object Bevel3: TBevel
        Left = 252
        Top = 4
        Width = 165
        Height = 129
      end
      object EditElement: TDBEdit
        Left = 108
        Top = 12
        Width = 133
        Height = 21
        DataSource = DSAccess
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object btnCrossEdit: TBitBtn
        Left = 424
        Top = 104
        Width = 141
        Height = 25
        Caption = '&Droits Utilisateurs'
        TabOrder = 8
        OnClick = btnCrossEditClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
          000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
          00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
          F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
          0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
          FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
          FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
          0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
          00333377737FFFFF773333303300000003333337337777777333}
        NumGlyphs = 2
      end
      object AccessKindSelecter: TDBRadioGroup
        Left = 256
        Top = 8
        Width = 157
        Height = 93
        Caption = 'Restrictions d'#39'acc'#232's'
        DataSource = DSAccess
        Items.Strings = (
          'Par n&iveau'
          'Par &utilisateur'
          'Par les &deux')
        TabOrder = 5
        Values.Strings = (
          'L'
          'U'
          'B')
      end
      object btnChangePassword: TBitBtn
        Left = 40
        Top = 104
        Width = 173
        Height = 25
        Caption = 'Modifier mot de &passe'
        TabOrder = 4
        OnClick = btnChangePasswordClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000003
          333333333F777773FF333333008888800333333377333F3773F3333077870787
          7033333733337F33373F3308888707888803337F33337F33337F330777880887
          7703337F33337FF3337F3308888000888803337F333777F3337F330777700077
          7703337F33377733337F33088888888888033373FFFFFFFFFF73333000000000
          00333337777777777733333308033308033333337F7F337F7F33333308033308
          033333337F7F337F7F33333308033308033333337F73FF737F33333377800087
          7333333373F77733733333333088888033333333373FFFF73333333333000003
          3333333333777773333333333333333333333333333333333333}
        NumGlyphs = 2
      end
      object MemoUsers: TDBMemo
        Left = 424
        Top = 8
        Width = 141
        Height = 93
        DataSource = DSAccess
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 7
      end
      object RadioButtonYes: TRadioButton
        Left = 144
        Top = 80
        Width = 37
        Height = 17
        Caption = '&Oui'
        TabOrder = 2
        OnClick = RadioButtonYesClick
      end
      object RadioButtonNo: TRadioButton
        Left = 184
        Top = 80
        Width = 41
        Height = 17
        Caption = '&Non'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = RadioButtonYesClick
      end
      object btnUsersFile: TBitBtn
        Left = 256
        Top = 104
        Width = 157
        Height = 25
        Caption = 'Fichier utilisateurs'
        TabOrder = 6
        OnClick = btnUsersFileClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00888888888888
          8888888888888888888888888888888888888888888880888888888888888008
          88888888888880E088888800000000EE0888880EEEEEEEEEE088880EEEEEEEEE
          E0888800000000EE08888888888880E088888888888880088888888888888088
          8888888888888888888888888888888888888888888888888888}
        Spacing = 2
      end
      object EditLevel: TDBComboBox
        Left = 144
        Top = 52
        Width = 81
        Height = 21
        DataSource = DSAccess
        ItemHeight = 13
        Items.Strings = (
          '1'
          '2'
          '3'
          '4')
        TabOrder = 1
      end
    end
    object Panel4: TPanel
      Left = 4
      Top = 166
      Width = 574
      Height = 168
      Align = alClient
      BevelOuter = bvLowered
      BorderWidth = 3
      Caption = 'Panel4'
      TabOrder = 1
      object GridElem: THDBGrid
        Left = 4
        Top = 4
        Width = 566
        Height = 160
        Align = alClient
        DataSource = DSAccess
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete]
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        SpHint = True
        AutoSizeLastColumn = True
        Columns = <
          item
            Expanded = False
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            Width = 65
            Visible = True
          end
          item
            Expanded = False
            Width = 418
            Visible = True
          end>
      end
    end
    object Panel5: TPanel
      Left = 4
      Top = 334
      Width = 574
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'Panel5'
      TabOrder = 2
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 0
        Width = 444
        Height = 26
        DataSource = DSAccess
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object DBInformation1: TDBRecordCount
        Left = 444
        Top = 0
        Width = 130
        Height = 26
        DataSource = DSAccess
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        RecordName = #233'l'#233'ment'
        PluralLetter = 's'
      end
    end
    object FindPanel1: TFindPanel
      Left = 4
      Top = 141
      Width = 574
      Height = 25
      Caption = ' Rechercher '
      VerticalAlignment = vaTop
      VisibleItems = [viButtons, viComboBox, viLabel]
      SearchField = 'Element'
      ChangeIndex = False
      ComboBoxWidth = 150
      Align = alTop
      TabOrder = 3
      TabStop = True
    end
  end
  object PanelBoutons: TPanel
    Left = 0
    Top = 364
    Width = 582
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'PanelBoutons'
    TabOrder = 1
    object DBNavBarre1: TDBNavBarre
      Left = 0
      Top = 0
      Width = 468
      Height = 40
      Align = alClient
      BevelOuter = bvLowered
      NavMode = nmSingleDatasource
      DataSource = DSAccess
      BtnSpacing = 6
      Spacing = 2
      MarginHeight = 3
      MarginWidth = 3
      VisibleBtn = [navEdit, navPost, navCancel]
      DeleteMessage = 'Effacer l'#39'enregistrement ?'
      FocusControl = EditElement
    end
    object Panel2: TPanel
      Left = 468
      Top = 0
      Width = 114
      Height = 40
      Align = alRight
      BevelOuter = bvLowered
      Caption = 'Panel2'
      TabOrder = 1
      object FermerBtn: TBitBtn
        Left = 3
        Top = 3
        Width = 108
        Height = 34
        Caption = '&Fermer'
        TabOrder = 0
        OnClick = FermerBtnClick
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          1800000000000006000000000000000000000000000000000000007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F007F7FFFFFFF007F7F00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F007F7FFFFFFF00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F7F7F7F007F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFFFFFFFF007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00000000FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7F7F
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF0000007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF00FFFF0000000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F7FFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F007F7F007F7F007F7F}
        NumGlyphs = 2
      end
    end
  end
  object DSAccess: TDataSource
    OnDataChange = DSAccessDataChange
    Left = 212
    Top = 228
  end
end
