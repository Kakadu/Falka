using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;


namespace FalkaCS {
    class Program {
        static void Main(string[] args) {
            var asm = Assembly.LoadFrom(@"..\..\..\Test\bin\Debug\Test.dll");
            var types = asm.GetTypes();
            var root = asm.GetType("Test+innerParser");

            var x = root.GetProperties().Select(pr => pr.GetCustomAttributes(false));
        }
        static void adasdf(dynamic x) {            x.foo();        }
    }
}
