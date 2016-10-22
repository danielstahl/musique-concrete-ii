package music

import music.ARControlInstrumentBuilder._
import music.LineControlInstrumentBuilder._
import net.soundmining.Instrument._
import net.soundmining.Utils._
import net.soundmining.{BusGenerator, Melody, MusicPlayer}

/**
  * Musique concrete 2
  */
object MusiqueConcrete2 {

  case class Sound(rawHarmonics: Seq[(Float, Float, Float)], harmonicsIndices: Seq[Integer], timeIndices: Seq[Integer], totalLengh: Float, bufNum: Integer) {
    val harmonics = harmonicsIndices.map(rawHarmonics(_)._3)
    val fundamental = harmonics.head
    val times = timeIndices.map(rawHarmonics(_)._1).sorted
  }

  val metalSound = Sound(
    rawHarmonics =
      Seq((2.720f, 3.065f, 2156.08f),
        (1.610f, 1.951f, 2165.86f),
        (0.266f, 0.607f, 3219.57f),
        (2.229f, 2.570f, 5437.65f),
        (2.229f, 2.570f, 7635.49f),
        (1.653f, 1.994f, 8748.63f),
        (3.018f, 3.359f, 11753.8f),
        (1.696f, 2.037f, 12398.7f),
        (2.250f, 2.591f, 13098.6f),
        (1.802f, 2.143f, 14481.8f),
        (2.400f, 2.741f, 16479.5f),
        (2.016f, 2.357f, 16482.5f),
        (2.250f, 2.591f, 16588.3f),
        (2.933f, 3.295f, 17369.7f),
        (2.656f, 2.997f, 17396.1f),
        (1.824f, 2.165f, 19730.9f)),
    harmonicsIndices = Seq(
      0,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      13,
      15
    ),
    timeIndices = Seq(
      0, 1, 2, 4, 6
    ),
    totalLengh = 4.649f,
    bufNum = 0)

  val rawStoneHarmonics: Seq[(Float, Float, Float)] = Seq(
    (2.144f, 2.485f, 193.994f),
    (2.144f, 2.455f, 258.019f),
    (2.698f, 3.039f, 283.765f),
    (0.288f, 0.629f, 304.951f),
    (3.040f, 3.381f, 317.06f),
    (2.144f, 2.485f, 319.173f),
    (1.632f, 1.994f, 327.25f),
    (0.778f, 1.119f, 349.04f),
    (3.466f, 3.807f, 360.478f),
    (2.677f, 3.018f, 380.074f),
    (0.309f, 0.650f, 693.977f),
    (0.788f, 0.619f, 958.288f),
    (2.677f, 3.039f, 991.561f),
    (2.677f, 3.039f, 1020.89f),
    (3.466f, 3.807f, 1057.71f),
    (0.309f, 0.650f, 1069.49f),
    (2.122f, 2.463f, 3565.24f),
    (2.101f, 2.442f, 3565.37f))

  val stereoFn = () => new PlayStereoSoundInstrumentBuilder
  val inverseStereoFn = () => new PlayStereoInverseSoundInstrumentBuilder

  def outline(sound: Sound)(implicit player: MusicPlayer): Unit = {

    val timeSortedHarmonics = sound.rawHarmonics.sortBy(_._1)

    timeSortedHarmonics.foreach {
      harmonic =>
        val (start, end, freq) = harmonic

        val dur = end - start

        val sine = new SineInstrumentBuilder()
          .addAction(TAIL_ACTION)
          .out(16)
          .dur(dur)
          .ampBus.control(ar(dur, 0.5f, (0, 1, 0)))
          .freqBus.control(line(dur, freq, freq))
          .buildInstruments()

        val pan = new PanInstrumentBuilder()
          .addAction(TAIL_ACTION)
          .dur(dur)
          .in(16)
          .out(0)
          .panBus.control(line(dur, 0, 0))
          .buildInstruments()

        player.sendNew(absoluteTimeToMillis(start), sine ++ pan)
    }
  }

  def playSound(bufNum: Integer, start: Float, factor: Float, instrument: () => CommonPlayStereoSoundInstrumentBuilder)(implicit player: MusicPlayer): Unit = {
    val longMetal1 = instrument.apply()
      .bufNum(bufNum)
      .amp(0.2f)
      .scale(factor)
      .start(0f)
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(start), longMetal1)
  }

  def playSoundMelody(start: Float, melodyIndices: Seq[Int], startTimeIndices: Seq[Int], instruments: Seq[() => CommonPlayStereoSoundInstrumentBuilder], inverterd: Boolean = true, sound: Sound)(implicit player: MusicPlayer): Unit = {
    val melody = melodyIndices.map(note => sound.harmonics(note))

    println(s"melody $melody")

    val factors =
      if (inverterd) melody.map(sound.fundamental / _)
      else melody.map(_ / sound.fundamental)


    println(s"factors $factors")

    val durations = factors.map(factor => sound.totalLengh / factor)

    val relativeStartTimes: Seq[Float] = startTimeIndices.map(sound.times(_))

    val scaledStartTimes = (relativeStartTimes, durations).zipped map ((relative, duration) => (relative / sound.totalLengh) * duration)
    println(s"scaledStartTimes $scaledStartTimes")

    val absoluteStartTimes = Melody.absolute(start, scaledStartTimes)
    println(s"absoluteStartTimes $absoluteStartTimes")

    (absoluteStartTimes, factors, instruments).zipped foreach ((startTime, factor, instrument) => playSound(sound.bufNum, startTime, factor, instrument))
  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    player.sendBundle(0, Seq(player.makeAllocRead(0, "/Users/danielstahl/Documents/Projects/musique-concrete-ii_sounds/long-metal-1.flac")))
    player.sendBundle(0, Seq(player.makeAllocRead(1, "/Users/danielstahl/Documents/Projects/musique-concrete-ii_sounds/long-stone-1.flac")))

    val fundamental = metalSound.fundamental
    val firstHarmonic = 1
    val secondHarmonic = 2
    val thirdHarmonic = 3
    val forthHarmonic = 4
    val fifthHarmonic = 5
    val sixthHarmonic = 6

    println(s"fundamental $fundamental totallen ${metalSound.totalLengh}")

    val melodyIndices = Seq(
      secondHarmonic, forthHarmonic, thirdHarmonic, fifthHarmonic, forthHarmonic, thirdHarmonic,
      sixthHarmonic, forthHarmonic, fifthHarmonic, thirdHarmonic, forthHarmonic, fifthHarmonic)

    val instruments = Seq(
      stereoFn, inverseStereoFn, stereoFn, inverseStereoFn, stereoFn, inverseStereoFn,
      stereoFn, inverseStereoFn, stereoFn, inverseStereoFn, stereoFn, inverseStereoFn
    )

    val firstTime = 0
    val secondTime = 1
    val thirdTime = 2
    val forthTime = 3
    val fifthTime = 4

    val relativeStartTimesIndices = Seq(thirdTime, secondTime, forthTime, fifthTime, secondTime, thirdTime, firstTime, forthTime, thirdTime, fifthTime, secondTime, thirdTime)
    println(s"relativeStartTimesIndices $relativeStartTimesIndices")


    playSoundMelody(0f, melodyIndices, relativeStartTimesIndices, instruments, sound = metalSound)
    playSoundMelody(0f, melodyIndices.reverse, relativeStartTimesIndices.reverse, instruments.reverse, sound = metalSound)

    //playMetal(0f, melodyIndices, relativeStartTimesIndices, instruments, inverterd = false)
    //playMetal(10f, melodyIndices.reverse, relativeStartTimesIndices.reverse, instruments.reverse, inverterd = false)

    Thread.sleep(5000)
  }
}
