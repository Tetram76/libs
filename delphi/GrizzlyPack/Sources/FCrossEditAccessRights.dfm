object FenCrossEditAccessRights: TFenCrossEditAccessRights
  Left = 319
  Top = 195
  Width = 460
  Height = 382
  Caption = 'Edition des droits d'#39'acc'#232's'
  Color = clBtnFace
  Constraints.MinHeight = 310
  Constraints.MinWidth = 460
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object DataGrid: THDBGrid
    Left = 0
    Top = 47
    Width = 452
    Height = 196
    Align = alClient
    DataSource = FInternalSource
    Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
    OnDblClick = DataGridDblClick
    OnKeyPress = DataGridKeyPress
    SpHint = True
    OnShowHint = DataGridShowHint
    OnSelectCell = DataGridSelectCell
  end
  object ExitPanel1: TExitPanel
    Left = 0
    Top = 314
    Width = 452
    Height = 41
    BtnHeight = 33
    BtnWidth = 90
    BtnOk.Caption = 'OK'
    BtnOk.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    BtnOk.ModalResult = 0
    BtnOk.Cancel = False
    BtnOk.Default = False
    BtnOk.Kind = bkCustom
    BtnCancel.Caption = 'Annuler'
    BtnCancel.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    BtnCancel.ModalResult = 2
    BtnCancel.Cancel = True
    BtnCancel.Default = False
    BtnCancel.Kind = bkCancel
    Spacing = 8
    Align = alBottom
    OnOkClick = ExitPanel1OkClick
  end
  object PanelSearch: TPanel
    Left = 0
    Top = 243
    Width = 452
    Height = 71
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    DesignSize = (
      452
      71)
    object Bevel1: TBevel
      Left = 128
      Top = 8
      Width = 319
      Height = 57
      Anchors = [akLeft, akTop, akRight]
    end
    object LabelTypeSearchString: TLabel
      Left = 199
      Top = 20
      Width = 184
      Height = 13
      Anchors = [akTop]
      Caption = 'Saisissez la cha'#238'ne '#224' rechercher'
    end
    object ElemSelecter: TComboBox
      Left = 199
      Top = 36
      Width = 189
      Height = 21
      Style = csDropDownList
      Anchors = [akTop]
      Color = clAqua
      DropDownCount = 12
      ItemHeight = 13
      TabOrder = 2
      Visible = False
      OnClick = ElemSelecterClick
    end
    object FindPanelUsers: TFindPanel
      Left = 199
      Top = 36
      Width = 189
      Height = 21
      Caption = ' Rechercher '
      FindMode = fmMultiWord
      DataSet = FVDataSet
      Interval = 1
      ChangeIndex = False
      ComboBoxWidth = 85
      BevelOuter = bvNone
      TabOrder = 1
      TabStop = True
      Anchors = [akTop]
    end
    object TypeSearchSelecter: TRadioGroup
      Left = 8
      Top = 4
      Width = 117
      Height = 61
      Caption = 'Rechercher'
      ItemIndex = 0
      Items.Strings = (
        'Un &utilisateur'
        'Un &'#233'l'#233'ment')
      TabOrder = 0
      OnClick = TypeSearchSelecterClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 47
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    DesignSize = (
      452
      47)
    object LabelSelectedElem: TLabel
      Left = 8
      Top = 4
      Width = 114
      Height = 13
      Caption = 'El'#233'ment s'#233'lectionn'#233
    end
    object LabelCurrentUser: TLabel
      Left = 224
      Top = 4
      Width = 57
      Height = 13
      Anchors = []
      Caption = 'Utilisateur'
    end
    object EditElem: THEdit
      Left = 8
      Top = 20
      Width = 177
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      SpHint = True
    end
    object EditElemNumber: THEdit
      Left = 188
      Top = 20
      Width = 29
      Height = 21
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      SpHint = True
    end
    object EditUser: THDBEdit
      Left = 224
      Top = 20
      Width = 105
      Height = 21
      Anchors = [akTop, akRight]
      DataSource = FInternalSource
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      SpHint = True
    end
    object AccessSelecter: TRadioGroup
      Left = 332
      Top = 4
      Width = 113
      Height = 37
      Anchors = [akTop, akRight]
      Caption = 'Acc'#232's autoris'#233
      Columns = 2
      Items.Strings = (
        '&Oui'
        '&Non')
      TabOrder = 3
      OnClick = AccessSelecterClick
    end
  end
  object FVDataSet: TVirtualDataSet
    IndexOptions = []
    AnsiCompare = False
    ExportFormat = efDataSet
    ExportCalcFields = False
    CachedUpdates = False
    Left = 336
    Top = 108
  end
  object FInternalSource: TDataSource
    DataSet = FVDataSet
    Left = 336
    Top = 144
  end
end
