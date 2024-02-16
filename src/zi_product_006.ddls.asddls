@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_PRODUCT_006'
define view entity ZI_PRODUCT_006 as select from zproduct006 as Product
association to parent ZI_PRICAT_006 as _Pricat on $projection.PricatUUID = _Pricat.PricatUUID
{
    key productuuid as ProductUUID,
    productid as ProductID,
    pricatuuid as PricatUUID,
    pricatgroupnumber as PricatGroupNumber,
    pricatname as PricatName,
    seriesname as SeriesName,
    articlename as ArticleName, 
    backsize as BackSize,
    cupsize as CupSize,
    color as Color,
    colorname as ColorName,
    gtin as GTIN,
    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    _Pricat
}
