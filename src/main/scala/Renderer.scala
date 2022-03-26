import scala.language.postfixOps
import scala._;
import javax.swing.JPanel
import java.awt.Dimension
import javax.swing.UIManager
import javax.swing.WindowConstants
import java.awt.Graphics
import java.awt.event.MouseListener
import javax.swing.JFrame
import java.awt.Event
import java.awt.event.MouseEvent

package renderer {
    import linalg._
    import sdf._
    import parsing.SceneParser
    import java.io.File
    import parsing.Geometry
    import java.nio.file.FileSystems
    import java.nio.file.Paths
    import java.nio.file.StandardWatchEventKinds
    import java.nio.file.WatchEvent
    
    class Renderer(viewport: (Float, Float, Float, Float), screen: (Float, Float), light: (Float, Float, Float)) {  
        import java.awt.image.BufferedImage
        import java.awt.Color
        import javax.imageio.ImageIO
        import java.io.File
        
        val MAX_STEPS = 1000
        val MAX_DISTANCE = 1000.0f
        val SURFACE_DISTANCE = 0.001f
        
        val pointMap = {
            import scala.collection.parallel._
            import scala.collection.parallel.CollectionConverters._
            
            println("VIEWPORT     : " + viewport)
            println("SCREEN       : " + screen)
            val viewportSize = (math.abs(viewport._1) + math.abs(viewport._3),
                                math.abs(viewport._2) + math.abs(viewport._4))
            println("VIEWPORT SIZE: " + viewportSize)

            val aspectRatio = (screen._1 / viewportSize._1, screen._2 / viewportSize._2)
            println("ASPECT RATIO :  " + aspectRatio)
            
            val delta = (viewportSize._1 / screen._1, viewportSize._2 / screen._2)
            println("DELTA        : " + delta)
            
            var map = new scala.collection.mutable.HashMap[(Int, Int), (Float, Float)]()
            var u = viewport._1
            while (u <= viewport._3) {
                var v = viewport._2
                while (v <= viewport._4) {
                    val x = ((u + viewportSize._1 / 2.0f) * aspectRatio._1).asInstanceOf[Int]
                    val y = ((v + viewportSize._2 / 2.0f) * aspectRatio._2).asInstanceOf[Int]
                    map.addOne(((x, y), (u, v)))
                    v += delta._2
                }
                u += delta._1
            }
            
            map.toList.toParArray
        }
        
        def raymarch(o: Observable, origin: Vec3, direction: Vec3): Distance = {
            var dO = 0.0f
            var i = 0
            while (i < MAX_STEPS) {
                val p = origin + direction * dO
                val ds = o(p)
                
                i += 1
                dO += ds
                
                if (dO > MAX_DISTANCE || ds < SURFACE_DISTANCE) return dO        
            }
            
            return dO
        }
        
        def normal(o: Observable, p: Vec3): Vec3 = { 
            val d = o(p)
            val delta = Vec3(o(p - Vec3(0.01f, 0.0f, 0.0f)),
                             o(p - Vec3(0.0f, 0.01f, 0.0f)),
                             o(p - Vec3(0.0f, 0.0f, 0.01f)))
            (Vec3(d, d, d) - delta).normalize()
        }

        def light(o: Observable, p: Vec3): Distance = {
            import linalg._

            val lightPos = Vec3(light._1, light._2, light._3)
            val l = normalize(lightPos - p)
            val n = normal(o, p)
        
            var dif = Util.clamp(dot(n, l), 0.0f, 1.0f)
        
            val d = raymarch(o, p + n * SURFACE_DISTANCE * 2.0f, l);             
            if(d < length(lightPos - p)) dif *= 0.01f;

            dif
        }

        def render(o: Observable, graphics: Graphics, setTitle: String => Unit) {
            import scala.collection.parallel._
            import scala.collection.parallel.CollectionConverters._
            import Util._
                        
            val forkJoinPool = java.util.concurrent.ForkJoinPool.commonPool()
            pointMap.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
            
            setTitle("Alloy [GROUPING BY DISTANCE]")
            
            val (points, t0) = time { pointMap.map { case ((x, y), (u, v)) => {
                val origin = Vec3(u, v, 0.0f)
                val dir = Vec3(u, v, 1).normalize()
                val d = raymarch(o, origin, dir)
                if (d >= MAX_DISTANCE) {
                    (x, y) -> 0.8f
                } else {
                    val p = origin + dir * d
                    val l = light(o, p)

                    (x, y) -> l
                }
            } } }

            val (groups, t1) = time { points.groupBy { case ((x, y), l) => {
                Util.clamp(l, 0.0f, 1.0f)
            } } }
            
            setTitle("Alloy [RENDERING]")

            val (_, t2) = time {
                groups.seq.map { case (c, pts) => {
                    graphics.setColor(new Color(c, c, c));
                    pts.map { case ((x, y), _) => {
                        graphics.drawLine(x, y, x, y)
                    } }
                } }
            }
            
            val elapsedTimeInSecond = (t1 + t2).asInstanceOf[Double] / 1_000_000_000.0;
            setTitle(s"Alloy [READY] | ${elapsedTimeInSecond} seconds")
        }
    }
    
    class RenderPanel(screen: (Float, Float)) extends JFrame with MouseListener {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
        
        val panel = new JPanel(true)

        panel.setPreferredSize(new Dimension(screen._1.asInstanceOf[Int], screen._2.asInstanceOf[Int]))
        add(panel)
        
        addMouseListener(this)
        
        pack()
        setTitle("Alloy [READY]")
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

        var shouldRepaint = false

        val watcherThread = new Thread(() => {
            val watcher = FileSystems.getDefault().newWatchService()
            val path = Paths.get("")
            path.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY)

            while(true) {
                var shouldRepaintAtEndOfFrame = false;
                var key = watcher.poll();
                while(key != null) {
                    key.pollEvents().toArray().foreach{ case event: WatchEvent[_] => {
                        if(event.context().toString().trim().endsWith(".alloy")) {
                            shouldRepaintAtEndOfFrame = true;
                        }
                    } }

                    key.reset()
                    key = watcher.poll()
                }

                if (shouldRepaintAtEndOfFrame) {
                    shouldRepaint = true;
                    repaint()
                }

                Thread.sleep(100)
            }
        });

        watcherThread.setDaemon(true)
        watcherThread.start()
        
        def mouseClicked(e: MouseEvent) {
            shouldRepaint = true
            this.repaint()
        }
        
        def mouseEntered(e: MouseEvent) {}
        def mouseExited(e: MouseEvent) {}
        def mousePressed(e: MouseEvent) {}
        def mouseReleased(e: MouseEvent) {}
        
        override def paint(graphics: Graphics) {
            import linalg._
            import sdf._
            
            if (shouldRepaint) {
                shouldRepaint = false

                val lines = scala.io.Source.fromFile("scene.alloy").mkString
                SceneParser.apply(lines) match {
                    case Left(arr) => {
                        println(s"SCENE: \n${arr}")

                        graphics.clearRect(0, 0, panel.getWidth(), panel.getHeight())
                        
                        val wh = (panel.getWidth().asInstanceOf[Float] / 100.0f, 
                                panel.getHeight().asInstanceOf[Float] / 100.0f)
                        val viewport = (-wh._1, -wh._2, wh._1, wh._2)
                        val light = (10.0f, 10.0f, 0.0f)

                        var renderer = new Renderer(viewport, screen, light);
                        
                        val head = arr.head
                        val tail = arr.tail
                        val scene: Observable = tail.foldLeft(head.emit())((obs, geo) => union((geo.emit(), obs)))
                        renderer.render(scene, graphics, (str) => { setTitle(str) })
                    }
                    case Right(msg) => println("Error!\n\n" + msg)
                }
            }
        }
    }
}