import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
 **/
 
class Sample  (var id :Int , var carriedBy:Int, var rank:Int ,var gain:String, var health:Int , var cost:Array[Int]){
     id: Int 
     carriedBy: Int 
     rank: Int 
     gain: String 
     health: Int 
     cost = new Array [Int](5)
}

class Robot(var storage :Array[Int] ,var target :String){
    storage = new Array [Int](5)
    target :String
}

object Player extends App {

    def gotoAndConnect(module :String, data: Char , position : String)  : Unit ={
        if(position == module){
            println("CONNECT " + data )
        }else{
            println("GOTO " + module )
        }
    }

    def gotoAndConnect2(module :String, data: Int , position : String)  : Unit ={
        if(position == module){
            println("CONNECT " + data)
        }else{
            println("GOTO " + module )
        }
    }

    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    // game loop
    while(true) {
        var samples = new mutable.ListBuffer[Sample]()
        var robots = new mutable.ListBuffer [Robot]()
        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt
            val expertiseB = _expertiseB.toInt
            val expertiseC = _expertiseC.toInt
            val expertiseD = _expertiseD.toInt
            val expertiseE = _expertiseE.toInt

            var storage = Array (storageA,storageB,storageC,storageD,storageE)
            var robot = new Robot(storage, target)
            robots += robot
        }

        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt
            val health = _health.toInt
            var costA = _costA.toInt 
            var costB = _costB.toInt
            var costC = _costC.toInt 
            var costD = _costD.toInt
            var costE = _costE.toInt 

            var cost = Array (costA,costB ,costC,costD, costE)
            var sample = new Sample(sampleId , carriedBy , rank, "expertiseGain", health, cost)
            samples += sample
             }

        var me = robots(0)
        var samp = samples(0)
        var bestSamplesList = new mutable.ListBuffer[Sample]()
        var best_sample :Sample = null
        var max_health = 0
        for (samp <- samples){
            if(samp.health > max_health &&  samp.carriedBy != 1 && bestSamplesList.size <= 3)
            best_sample = samp
            max_health = samp.health
            bestSamplesList += best_sample
        }

        if(best_sample.carriedBy != 0){
            gotoAndConnect2("DIAGNOSIS",best_sample.id, me.target)
        }else{
            if(me.storage(0) < best_sample.cost(0)){
                var c :Char = 'A'
                gotoAndConnect("MOLECULES",c, me.target)   
            }else if(me.storage(1) < best_sample.cost(1)){
                var c :Char= 'B'
                gotoAndConnect("MOLECULES",c, me.target)   
            }else if(me.storage(2) < best_sample.cost(2)){
                var c :Char = 'C'
                gotoAndConnect("MOLECULES",c, me.target)   
            }else if(me.storage(3) < best_sample.cost(3)){
                var c :Char = 'D'
                gotoAndConnect("MOLECULES",c, me.target)   
            }else if(me.storage(4) < best_sample.cost(4)){
                var c :Char = 'E'
                gotoAndConnect("MOLECULES",c, me.target)   
            }else{
                var c :Char ='W'
                 gotoAndConnect2("LABORATORY", best_sample.id, me.target)
            }
        } 
        }
    }
