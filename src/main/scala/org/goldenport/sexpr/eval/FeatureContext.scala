package org.goldenport.sexpr.eval

import org.goldenport.hocon.RichConfig
import org.goldenport.record.v3.sql.SqlContext
import store.StoreFeature
import chart.ChartFeature

/*
 * @since   Mar. 10, 2019
 * @version Apr.  6, 2019
 * @author  ASAMI, Tomoharu
 */
case class FeatureContext(
  store: StoreFeature,
  chart: ChartFeature
)

object FeatureContext {
  def default = FeatureContext(StoreFeature.empty, ChartFeature.default)

  def create(config: RichConfig, sqlcontext: SqlContext) =
    FeatureContext(new StoreFeature(config, sqlcontext), ChartFeature.default)
}
