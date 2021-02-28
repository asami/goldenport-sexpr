package org.goldenport.sexpr.eval

import org.goldenport.hocon.RichConfig
import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.sql.SqlContext
import store.StoreFeature
import chart.ChartFeature

/*
 * @since   Mar. 10, 2019
 *  version Apr.  6, 2019
 *  version Mar. 30, 2020
 * @version Feb. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class FeatureContext(
  store: StoreFeature,
  chart: ChartFeature
)

object FeatureContext {
  def now() = FeatureContext(StoreFeature.now(), ChartFeature.default)

  def create(config: RichConfig, i18ncontext: I18NContext, sqlcontext: SqlContext) =
    FeatureContext(new StoreFeature(config, i18ncontext, sqlcontext), ChartFeature.default)
}
