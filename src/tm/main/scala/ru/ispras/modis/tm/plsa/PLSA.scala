package ru.ispras.modis.tm.plsa

import ru.ispras.modis.tm.attribute.AttributeType
import ru.ispras.modis.tm.brick.AbstractPLSABrick
import ru.ispras.modis.tm.documents.{Alphabet, Document}
import ru.ispras.modis.tm.matrix.{AttributedPhi, Theta}
import ru.ispras.modis.tm.regularizer.Regularizer
import ru.ispras.modis.tm.sparsifier.Sparsifier
import ru.ispras.modis.tm.stoppingcriteria.StoppingCriteria
import ru.ispras.modis.tm.utils.{ElapsedTime, TopicHelper}

/**
 * Created with IntelliJ IDEA.
 * User: padre
 * Date: 27.03.14
 * Time: 16:37
 */
/**
 * main part of all project. It class put all bricks together and process training algorithm
 * @param bricks plsaBrick to process one attribute
 * @param stoppingCriteria check is it time to stop
 * @param thetaSparsifier sparsify matrix theta
 * @param regularizer regularizer, for example direchlet
 * @param documents collection used for training
 * @param phi words by topic distribution matrix
 * @param theta document by topic distribution matrix
 */
class PLSA(private val bricks: Map[AttributeType, AbstractPLSABrick],
           private val stoppingCriteria: StoppingCriteria,
           private val thetaSparsifier: Sparsifier,
           private val regularizer: Regularizer,
           private val documents: Array[Document],
           private val phi: Map[AttributeType, AttributedPhi],
           private val theta: Theta,
           private val alphabet: Alphabet) extends ElapsedTime  {

    /**
     * take sequence of documents into input and train model on it
     * @return trained model (matrix phi and theta)
     */
    def train: TrainedModel = {
        val collectionLength = documents.foldLeft(0) {
            (sum, document) => sum + document.numberOfWords()
        }
        var numberOfIteration = 0
        var oldPpx = 0d
        var newPpx = 0d
        while (!stoppingCriteria(numberOfIteration, oldPpx, newPpx)) time("iteration")  {
            oldPpx = newPpx
            newPpx = makeIteration(numberOfIteration, collectionLength, documents)
            numberOfIteration += 1
        }

        new TrainedModel(phi, theta, newPpx, bricks.map{case(attr, brick) => (attr, brick.attributeWeight)}, alphabet.toSeq, bricks.head._2.getBackground().get)
    }

    def logLikelihood(param: Array[Double]): Double = {
        val theta1 = Theta(Array(param.map(_.toFloat)))
        bricks.foldLeft(0d) { case (sum, (attribute, brick)) =>
            sum + brick.makeIteration(theta1, phi(attribute), documents, 0)
        }
    }

    def predictTheta(): (Double, Array[Double]) = {

        var res = 0.0

        for (iter <- 0 until 5) {

            val logLikelihood = bricks.foldLeft(0d) { case (sum, (attribute, brick)) =>
                sum + brick.makeIteration(theta, phi(attribute), documents, 0)
            }

            applyRegularizer()
            theta.dump()
            theta.sparsify(thetaSparsifier, iter)

            // println(logLikelihood)
            res = logLikelihood

        }

        (res, theta.getRow(0).map(_.toDouble))
    }

    /**
     * calculate perplexity by given loglikelihood and length  of collection
     * @param logLikelihood logarithm of likelihood to observe collection: ln(p(D\ Phi, Theta))
     * @param collectionLength number of words in collection
     * @return perplexity of model
     */
    protected def perplexity(logLikelihood: Double, collectionLength: Int) = math.exp(-logLikelihood / collectionLength)

    /**
     * perform iteration
     * @param iterationCnt serial number of iteration
     * @param collectionLength number of words in collection
     * @param documents sequence of input documents
     * @return perplexity of model after iteration
     */
    protected def makeIteration(iterationCnt: Int, collectionLength: Int, documents: Array[Document]) = {
        val logLikelihood = bricks.foldLeft(0d) { case (sum, (attribute, brick)) =>
            sum + brick.makeIteration(theta, phi(attribute), documents, iterationCnt)
        }
        val newPpx = perplexity(logLikelihood, collectionLength)
        info("number of iteration = " + iterationCnt)
        println("number of iteration = " + iterationCnt)
        info("perplexity = " + newPpx)
        info("number of significant topics = " + TopicHelper.getSignificantTopics(theta).size)
        applyRegularizer()
        theta.dump()
        theta.sparsify(thetaSparsifier, iterationCnt)
        newPpx
    }

    /**
     * apply regularizer to matrix theta
     */
    private def applyRegularizer() {
        regularizer.regularizeTheta(phi, theta)
    }
}
