package org.goldenport.sexpr.eval

import org.goldenport.hocon.RichConfig
import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.store.StoreFactory
import org.goldenport.record.store.Query
import store.StoreFeature
import entity.{EntityFeature, EntityFactory}
import chart.ChartFeature

/*
 * @since   Mar. 10, 2019
 *  version Apr.  6, 2019
 *  version Mar. 30, 2020
 *  version Feb. 20, 2021
 *  version Sep. 20, 2021
 *  version Oct.  3, 2021
 * @version Sep.  7, 2024
 * @author  ASAMI, Tomoharu
 */
case class FeatureContext(
  store: StoreFeature,
  entity: EntityFeature,
  chart: ChartFeature
) {
  def sqlContext = store.sqlContext
}

object FeatureContext {
  def create(
    config: RichConfig,
    i18ncontext: I18NContext,
    sqlcontext: SqlContext,
    entityfactory: EntityFactory
  ) = {
    val factory = new StoreFactory(config, sqlcontext)
    val store = new StoreFeature(config, i18ncontext, sqlcontext, factory)
    val entity = EntityFeature.create(entityfactory)
    FeatureContext(store, entity, ChartFeature.default)
  }

  def createForTest(i18ncontext: I18NContext) = {
    val properties = RichConfig.empty
    val querycontext = Query.Context.now()
    val sqlcontext = SqlContext.createEachTime(properties, querycontext)
    val entityfactory = EntityFactory.empty
    create(RichConfig.empty, i18ncontext, sqlcontext, entityfactory)
  }
}
