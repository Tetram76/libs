object RepRequetesLibres: TRepRequetesLibres
  Tag = 1
  Left = 0
  Top = 0
  Width = 952
  Height = 1347
  Frame.Color = clBlack
  Frame.DrawTop = False
  Frame.DrawBottom = False
  Frame.DrawLeft = False
  Frame.DrawRight = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  Functions.Strings = (
    'PAGENUMBER'
    'COLUMNNUMBER'
    'REPORTTITLE')
  Functions.DATA = (
    '0'
    '0'
    #39#39)
  OnEndPage = QuickRepEndPage
  Options = [FirstPageHeader, LastPageFooter]
  Page.Columns = 1
  Page.Orientation = poPortrait
  Page.PaperSize = A4
  Page.Values = (
    150.000000000000000000
    2970.000000000000000000
    150.000000000000000000
    2100.000000000000000000
    150.000000000000000000
    150.000000000000000000
    0.000000000000000000)
  PrinterSettings.Copies = 1
  PrinterSettings.OutputBin = First
  PrinterSettings.Duplex = False
  PrinterSettings.FirstPage = 0
  PrinterSettings.LastPage = 0
  PrinterSettings.ExtendedDuplex = 0
  PrinterSettings.UseStandardprinter = False
  PrinterSettings.UseCustomBinCode = False
  PrinterSettings.CustomBinCode = 0
  PrinterSettings.UseCustomPaperCode = False
  PrinterSettings.CustomPaperCode = 0
  PrinterSettings.PrintMetaFile = False
  PrintIfEmpty = False
  ReportTitle = 'Requ'#234'tes libres'
  SnapToGrid = True
  Units = MM
  Zoom = 120
  PrevFormStyle = fsNormal
  PreviewInitialState = wsNormal
  object DetailBand1: TQRBand
    Left = 68
    Top = 133
    Width = 816
    Height = 17
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      37.482638888888890000
      1799.166666666667000000)
    BandType = rbDetail
    object GridDetail: TQRCustomGrid
      Left = 0
      Top = 0
      Width = 64
      Height = 17
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = True
      Frame.DrawLeft = True
      Frame.DrawRight = True
      Size.Values = (
        37.482638888888900000
        0.000000000000000000
        0.000000000000000000
        141.111111111111100000)
      Alignment = taLeftJustify
      AlignToBand = True
      LabelKind = lkFieldValue
      Columns = <>
      Color = clWhite
    end
  end
  object PageFooterBand1: TQRBand
    Left = 68
    Top = 150
    Width = 816
    Height = 29
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      63.940972222222230000
      1799.166666666667000000)
    BandType = rbPageFooter
    object QRSysData2: TQRSysData
      Left = 0
      Top = 5
      Width = 107
      Height = 20
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        44.097222222222230000
        0.000000000000000000
        11.024305555555560000
        235.920138888888900000)
      Alignment = taLeftJustify
      AlignToBand = True
      AutoSize = True
      Color = clWhite
      Data = qrsPageNumber
      Text = 'Page '
      Transparent = False
      FontSize = 10
    end
    object QRSysData3: TQRSysData
      Left = 671
      Top = 5
      Width = 145
      Height = 20
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        44.097222222222230000
        1479.461805555556000000
        11.024305555555560000
        319.704861111111200000)
      Alignment = taRightJustify
      AlignToBand = True
      AutoSize = True
      Color = clWhite
      Data = qrsDetailNo
      OnPrint = QRSysData3Print
      Text = 'D'#233'compte '
      Transparent = False
      FontSize = 10
    end
  end
  object TitleBand1: TQRBand
    Left = 68
    Top = 68
    Width = 816
    Height = 48
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      105.833333333333300000
      1799.166666666667000000)
    BandType = rbTitle
    object QRTitre: TQRSysData
      Left = 358
      Top = 1
      Width = 100
      Height = 38
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        83.784722222222230000
        789.340277777777800000
        2.204861111111111000
        220.486111111111200000)
      Alignment = taCenter
      AlignToBand = True
      AutoSize = True
      Color = clWhite
      Data = qrsReportTitle
      Transparent = False
      FontSize = 10
    end
    object QRSysData4: TQRSysData
      Left = 0
      Top = 5
      Width = 120
      Height = 20
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        44.097222222222230000
        0.000000000000000000
        11.024305555555560000
        264.583333333333400000)
      Alignment = taLeftJustify
      AlignToBand = True
      AutoSize = True
      Color = clWhite
      Data = qrsDate
      Text = 'Imprim'#233' le '
      Transparent = False
      FontSize = 10
    end
  end
  object ColumnHeaderBand1: TQRBand
    Left = 68
    Top = 116
    Width = 816
    Height = 17
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      37.482638888888890000
      1799.166666666667000000)
    BandType = rbColumnHeader
    object GridTitres: TQRCustomGrid
      Left = 0
      Top = 0
      Width = 64
      Height = 17
      Frame.Color = clBlack
      Frame.DrawTop = True
      Frame.DrawBottom = True
      Frame.DrawLeft = True
      Frame.DrawRight = True
      Size.Values = (
        37.482638888888900000
        0.000000000000000000
        0.000000000000000000
        141.111111111111100000)
      Alignment = taLeftJustify
      AlignToBand = True
      LabelKind = lkFieldLabel
      Columns = <>
      Color = clWhite
    end
  end
end
