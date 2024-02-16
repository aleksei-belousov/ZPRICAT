@EndUserText.label: 'ZC_PRODUCT_006'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_PRODUCT_006 as projection on ZI_PRODUCT_006
{
    key ProductUUID,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_ProductTP_2', element: 'Product' } } ] 
    @EndUserText.label: 'Product'
    ProductID,
    PricatUUID,

    PricatGroupNumber,

    PricatName,

    SeriesName,
    ArticleName, 
    BackSize,
    CupSize,
    Color,
    ColorName,
    GTIN,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    _Pricat : redirected to parent ZC_PRICAT_006
}
