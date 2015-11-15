package ru.ispras.modis.tm.regularizer


import ru.ispras.modis.tm.attribute.AttributeType
import ru.ispras.modis.tm.attribute.AttributeType
import ru.ispras.modis.tm.documents.Document
import ru.ispras.modis.tm.documents.Document
import ru.ispras.modis.tm.matrix.AttributedPhi
import ru.ispras.modis.tm.matrix.ImmutablePhi
import ru.ispras.modis.tm.matrix.ImmutableTheta
import ru.ispras.modis.tm.matrix.Theta
import ru.ispras.modis.tm.matrix.{AttributedPhi, ImmutablePhi, ImmutableTheta, Theta}

class PredefThetaReg(private val regularizationParameter: Float, val theta0: Array[Array[Float]]) extends Regularizer {
  require(regularizationParameter >= 0, "regularizationParameter  should be >= 0")


  def apply(phi: Map[AttributeType, AttributedPhi], theta: Theta): Float = {
    0f
  }


  override private[regularizer] def regularizeThetaImmutable(phi: Map[AttributeType, ImmutablePhi], theta: Theta): Unit = {

    assert(theta.numberOfTopics == theta0(0).length)

    theta.addToExpectation { (docInd, topic) =>
      regularizationParameter * theta0(docInd)(topic)
    }
  }

  override private[regularizer] def regularizePhiImmutable(phi: AttributedPhi, theta: ImmutableTheta): Unit = {}



}
