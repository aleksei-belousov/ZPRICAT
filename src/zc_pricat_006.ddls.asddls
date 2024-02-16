@EndUserText.label: 'ZC_PRICAT_006'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZC_PRICAT_006 provider contract transactional_query as projection on ZI_PRICAT_006
{
    key PricatUUID,
    PricatID,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_SalesOrganization', element: 'SalesOrganization' }, useForValidation: true } ]
    @ObjectModel.foreignKey.association: '_SalesOrganization'
    @EndUserText.label: 'Sales Organization'
    SalesOrganization,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_DistributionChannel', element: 'DistributionChannel' }, useForValidation: true } ]
    @ObjectModel.foreignKey.association: '_DistributionChannel'
    @EndUserText.label: 'Distribution Channel'
    DistributionChannel,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Division', element: 'Division' }, useForValidation: true } ]
    @ObjectModel.foreignKey.association: '_Division'
    @EndUserText.label: 'Organization Division'
    Division,

    ProductSeries,

    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,

    /* Associations */
    _Customer:  redirected to composition child ZC_CUSTOMER_006,
    _Product:   redirected to composition child ZC_PRODUCT_006,
    _SalesOrganization,
    _DistributionChannel,
    _Division

}
