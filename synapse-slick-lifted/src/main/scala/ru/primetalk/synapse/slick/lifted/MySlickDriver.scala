package ru.primetalk.synapse.slick.lifted

//import com.github.tminglei.slickpg.PgArraySupport

import slick.driver.PostgresDriver


trait SomeFeatureSupport {

}
/**
 * @author zhizhelev, 17.02.15.
 */
class MySlickDriver extends PostgresDriver //with PgArraySupport
with SomeFeatureSupport {

}
