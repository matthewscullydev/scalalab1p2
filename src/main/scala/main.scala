package lab1
class ThrownException extends Exception("exception")
// problem 1  ** important note is that order matters in this situation. The if statement with the most operators (case b) is evaluated first so it must
//               be placed above the case a statement in this example

// problem solving note : when given a prompt with sub-setting conditions evaluate the outermost set first.
/*
def kingdom(n: Int): Int = {
    n match
      case b if (n > 10 && n % 2 == 0 && n % 10 == 0) => 2
      case a if (n > 10 && n % 2 == 0) => 1
      case c if (n <= 10) => 3
      case d => 4
}
*/

def kingdom(n: Int) : Int = {
  if(n > 10)
     if(n%2==0)
        if(n%100==0)
          2
        else 1
     else 4
  else 3
}

 def testKingdom() =
	println(s"kingdom(5) = ${kingdom(5)}") // 3
	println(s"kingdom(11) = ${kingdom(11)}") // 4
	println(s"kingdom(16) = ${kingdom(16)}") // 1
	println(s"kingdom(200) = ${kingdom(200)}") // 2


// problem 2

def order(n: Int): Int = {
    if(n <= 0){
      0
    }
    else{
      family(n) * ilk(n) + genus(n)
    }
}
def family(n: Int) = {
    if( n % 3 == 0) {
      1
    }
    else {
      2
    }
}
def ilk(n: Int) = {
    if ( n == 50){
      3
    }
    else {
      4
    }
}

def genus(n: Int) = {
    if( n % 7 == 0){
      5
    }
    else {
      6
    }
}

 def testOrder() =
	println(s"order(-1) = ${order(-1)}") // 0
	println(s"order(15) = ${order(15)}") // 10
	println(s"order(50) = ${order(50)}") // 12
	println(s"order(49) = ${order(49)}") // 13
	println(s"order(21) = ${order(21)}") // 9


// problem 3

// original version issue: needs accompanying else block for first if case

// corrected version adds accompanying else block for first if case


def species(n: Int) = {
  if (0 < n) if (n % 2 == 0) 1 else 2
}

def species2(n: Int) = {
  if (n > 0)
    if (n % 2 == 0)
      1
    else 2
  else 2
}

 def testSpecies() =
	println(s"species(0) = ${species(0)}") // = () i.e. undspecified
	println(s"species2(0) = ${species2(0)}") // = 2 as specified


// problem 4

// odd positives are realm 1



def realm1(n: Int): Int = {
  if(n > 0)
    if(n % 2 == 0)
      0
    else 1
  else 0

}


// even positives not divisible by 3 are realm 2
// another style: check for bad news first
def realm2(n: Int): Int = {
    if(n % 3 == 0){
      0
    }
    else
      2
}

// even positives divisible by 6 and 7 are realm 3
def realm3(n: Int): Int = {
    if (n % 6 == 0){
      3
    }
    else if(n % 7 == 0){
      3
    }
    else
      0
}

def realm(n: Int): Int = {

  if (realm1(n) == 0)
    if (realm2(n) == 0)
      if(realm3(n) == 0)
          realm3(n)
      else if(realm3(n) == 3)
        if(n == 0){
          0
        }
        else
          realm3(n)
      else
        realm2(n)
    else
      realm2(n)
  else
    realm1(n)
}

 def testRealm() =
  println(s"realm(0) = ${realm(0)}") // 0
  println(s"realm(5) = ${realm(4)}") // 2
  println(s"realm(42) = ${realm(42)}") // 3
  println(s"realm(9) = ${realm(9)}") // 1



  /*
  if (realm2(n) == 0 && realm3(n) == 3 && realm1(n) == 0 && n != 0) {
    realm3(n)
  }
  else if (realm2(n) == 2) {
    realm2(n)
  }
  else {
    realm1(n)
  }

}
*/



// problem 6

// odd positives are realm 1
def realm1Opt(n: Int): Option[Int] =
	if (n > 0)
		if (n % 2 == 0) Some(0)
		else Some(1)
	else Some(0)


// even positives not divisible by 3 are realm 2
// another style: check for bad news first
def realm2Opt(n: Int): Option[Int] = {

  if (n % 3 == 0)
    Some(0)
  else
    Some(2)
}
// even positives divisible by 6 and 7 are realm 3
def realm3Opt(n: Int): Option[Int] = {
  if (n % 6 == 0) {
    Some(3)
  }
  else if (n % 7 == 0) {
    Some(3)
  }
  else
    Some(0)
}
def realmOpt(n: Int) = {
  if (realm1Opt(n) == Some(0))
    if (realm2Opt(n) == Some(0))
      if (realm3Opt(n) == Some(0))
        realm3Opt(n)
      else if (realm3Opt(n) == Some(3))
        if (n == 0) {
          Some(0)
        }
        else
          realm3Opt(n)
      else
        realm2Opt(n)
    else
      realm2Opt(n)
  else
    realm1Opt(n)
}

 def testRealmOpt() =
	println(s"realmOpt(0) = ${realmOpt(0)}") // 0
	println(s"realmOpt(5) = ${realmOpt(4)}") // 2
	println(s"realmOpt(42) = ${realmOpt(42)}") // 3
	println(s"realmOpt(9) = ${realmOpt(9)}") // 1


@main def numerologyTest() =
	testSpecies()
	testKingdom()
	testOrder()
	testRealm()
	testRealmOpt()


