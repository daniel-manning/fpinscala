import java.util.concurrent.{ExecutorService, Future}

package object chapter07 {

  type Par[A] = ExecutorService => Future[A]

}
