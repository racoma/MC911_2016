
pop: proc () returns (int loc);
    if s[top+1] == 0 then
        print("empty stack");
	result 0;
    else
        result s[s[top+1]];
	s[top+1] -= 1;
    fi;
end;
