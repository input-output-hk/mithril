use mithril_aggregator::MetricsService;

pub struct MetricsValidator<'a> {
    metrics_service: &'a MetricsService,
    certificate_total: Option<u32>,
    artifact_cardano_db_total: Option<u32>,
    artifact_mithril_stake_distribution_total: Option<u32>,
    artifact_cardano_transaction_total: Option<u32>,
    artifact_cardano_stake_distribution_total: Option<u32>,
}

impl<'a> MetricsValidator<'a> {
    pub fn validate(&self) {
        if let Some(expected) = self.certificate_total {
            assert_eq!(
                expected,
                self.metrics_service
                    .get_certificate_total_produced_since_startup()
                    .get()
            );
        }

        if let Some(expected) = self.artifact_cardano_db_total {
            assert_eq!(
                expected,
                self.metrics_service
                    .get_artifact_cardano_db_total_produced_since_startup()
                    .get()
            );
        }

        if let Some(expected) = self.artifact_mithril_stake_distribution_total {
            assert_eq!(
                expected,
                self.metrics_service
                    .get_artifact_mithril_stake_distribution_total_produced_since_startup()
                    .get()
            );
        }

        if let Some(expected) = self.artifact_cardano_transaction_total {
            assert_eq!(
                expected,
                self.metrics_service
                    .get_artifact_cardano_transaction_total_produced_since_startup()
                    .get()
            );
        }

        if let Some(expected) = self.artifact_cardano_stake_distribution_total {
            assert_eq!(
                expected,
                self.metrics_service
                    .get_artifact_cardano_stake_distribution_total_produced_since_startup()
                    .get()
            );
        }
    }
}

pub struct MetricsValidatorBuilder<'a> {
    metrics_service: &'a MetricsService,
    certificate_total: Option<u32>,
    artifact_cardano_db_total: Option<u32>,
    artifact_mithril_stake_distribution_total: Option<u32>,
    artifact_cardano_transaction_total: Option<u32>,
    artifact_cardano_stake_distribution_total: Option<u32>,
}

impl<'a> MetricsValidatorBuilder<'a> {
    pub fn new(metrics_service: &'a MetricsService) -> Self {
        Self {
            metrics_service,
            certificate_total: None,
            artifact_cardano_db_total: None,
            artifact_mithril_stake_distribution_total: None,
            artifact_cardano_transaction_total: None,
            artifact_cardano_stake_distribution_total: None,
        }
    }

    pub fn certificate_total(mut self, value: u32) -> Self {
        self.certificate_total = Some(value);
        self
    }

    pub fn artifact_cardano_db_total(mut self, value: u32) -> Self {
        self.artifact_cardano_db_total = Some(value);
        self
    }

    pub fn artifact_mithril_stake_distribution_total(mut self, value: u32) -> Self {
        self.artifact_mithril_stake_distribution_total = Some(value);
        self
    }

    pub fn artifact_cardano_transaction_total(mut self, value: u32) -> Self {
        self.artifact_cardano_transaction_total = Some(value);
        self
    }

    pub fn artifact_cardano_stake_distribution_total(mut self, value: u32) -> Self {
        self.artifact_cardano_stake_distribution_total = Some(value);
        self
    }

    pub fn build(self) -> MetricsValidator<'a> {
        MetricsValidator {
            metrics_service: self.metrics_service,
            certificate_total: self.certificate_total,
            artifact_cardano_db_total: self.artifact_cardano_db_total,
            artifact_mithril_stake_distribution_total: self
                .artifact_mithril_stake_distribution_total,
            artifact_cardano_transaction_total: self.artifact_cardano_transaction_total,
            artifact_cardano_stake_distribution_total: self
                .artifact_cardano_stake_distribution_total,
        }
    }
}
