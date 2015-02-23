package main

import (
	"flag"
	"fmt"
	"github.com/danmane/abalone/go/operator"
)

func main() {
	flag.Parse()
	scheduler := operator.NewScheduler(14000, 100)
	fmt.Println(flag.Arg(0))
	err := operator.Validate(flag.Arg(0), scheduler)
	if err != nil {
		fmt.Println("Validation Failed!")
		fmt.Println(err)
	} else {
		fmt.Println("Validation succeeded")
	}
}
