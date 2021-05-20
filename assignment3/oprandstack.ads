with SimpleStack;

package OprandStack is new SimpleStack(Max_Size => 512,
                                       Item => Integer,
                                       Default_Item => 0);
