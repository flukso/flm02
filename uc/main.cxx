int main(void)
{
	WDT_off();

//	init();

	setup();
    
	for (;;)
		loop();
        
	return 0;
}

