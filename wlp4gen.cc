#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
using namespace std;

int location = 1;
int offset = 0;
int num_if = 0;
int num_loop = 0;
int num_delete = 0;
int num_arg = 0;
int num_arg_copy = 0;
map<string, pair<vector<string>, map<string, string>>> tables;

struct Node 
{ 
    string key; 
    string lexeme;
    vector<Node> child; 
}; 

struct var {
    string name;
    string type;
    int location;
    int offset;
};

void create_tree(Node &curr, vector<string> t) {
    string line;
    stringstream ss;
    string word;
    getline(cin, line);
    curr.key = line;
    ss << line;
    ss >> word;
    for(int i = 0; i < t.size(); i++) {
        if(word == t[i]) { // we get a terminal
            ss >> word;
            curr.lexeme = word;
            return;
        }
    }
    
    while(ss >> word) { 
        vector<Node> n;
        string my_key = word;
        string lex = "";
        Node my_insert = Node{my_key, lex, n};
        curr.child.push_back(my_insert);
    }

    for(int i = 0; i < curr.child.size(); i++) {
        create_tree(curr.child[i], t);
        
    }

}

void print_tree(Node my_tree) {
    cout << my_tree.key << " " << my_tree.lexeme;
    for(int i = 0; i < my_tree.child.size(); i++) {
        cout << my_tree.child[i].key << " ";
    }
    cout << endl;
    
    for(int i = 0; i < my_tree.child.size();) {
        if(my_tree.child.size() == 0) {
            return;
        } else {
            print_tree(my_tree.child[i]);
            i++;
        }
    }
}

string check_type(Node &my_tree, map<string, pair<vector<string>, map<string, string>>> &t, string sc) {
    if(my_tree.key == "procedures procedure procedures") {
        check_type(my_tree.child[0], t, sc);
        check_type(my_tree.child[1], t, sc);
        return "int";
    } else if(my_tree.key == "procedures main") {
        
        return check_type(my_tree.child[0], t, sc);
    } else if(my_tree.key == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        check_type(my_tree.child[7], t, my_tree.child[1].lexeme);
        check_type(my_tree.child[9], t, my_tree.child[1].lexeme);
        return "int";
    } else if(my_tree.key == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        check_type(my_tree.child[9], t, "wain");
        check_type(my_tree.child[11], t, "wain");
        return "int";
    
    /***************************  expr ************************/
    } else if(my_tree.key == "expr term") {
         return check_type(my_tree.child[0], t, sc);
    } else if(my_tree.key == "expr expr PLUS term") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int*" && right == "int*") {
            string error = "ERROR: adding two pointers";
            throw error;
        } else if(left == "int*" && right == "int") {
            return "int*";
        } else if(left == "int" && right == "int*") {
            return "int*";
        } else if(left == "int" && right == "int") {
            return "int";
        }
    } else if(my_tree.key == "expr expr MINUS term") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int" && right == "int") {
            return "int";
        } else if(left == "int*" && right == "int") {
            return "int*";
        } else if(left == "int*" && right == "int*") {
            return "int";
        } else {
            string error = "ERROR: MINUS";
            throw error;
        }
    /************************** term ***********************/
    } else if(my_tree.key == "term factor") {
         return check_type(my_tree.child[0], t, sc);
    } else if(my_tree.key == "term term STAR factor") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int" && right == "int") {
            return "int";
        } else {
            string error = "ERROR: term term STAR factor";
            throw error;
        }
    } else if(my_tree.key == "term term SLASH factor") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int" && right == "int") {
            return "int";
        } else {
            string error = "ERROR: term term SLASH factor";
            throw error;
        }
    } else if(my_tree.key == "term term PCT factor") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int" && right == "int") {
            return "int";
        } else {
            string error = "ERROR: term term PCT factor";
            throw error;
        }
    /************************* factor ************************/
    } else if(my_tree.key == "factor ID") {
        bool found = 0;
        for(auto each:t[sc].second) {
            if(my_tree.child[0].lexeme == each.first) {
                if(each.second == "int*") {
                    found = 1;
                    return "int*";
                    
                } else if(each.second == "int") {
                    found = 1;
                    return "int";
                    
                }
                
            }
        }
        if(found == 0) {
            string error = "ERROR: 404 ID";
            throw error;
        }
    } else if(my_tree.key == "factor NUM") {
        return "int";
    } else if(my_tree.key == "factor NULL") {
        return "int*";
    } else if(my_tree.key == "factor LPAREN expr RPAREN") {
        string expr = check_type(my_tree.child[1], t, sc);
        return expr;
    } else if(my_tree.key == "factor AMP lvalue") {
        string lvalue = check_type(my_tree.child[1], t, sc);
        if(lvalue == "int") {
            return "int*";
        } else {
            string error = "ERROR: factor AMP lvalue";
            throw error;
        }
    } else if(my_tree.key == "factor STAR factor") {
        string fac = check_type(my_tree.child[1], t, sc);
        if(fac == "int*") {
            return "int";
        } else {
            string error = "ERROR: factor STAR factor";
            throw error;
        }
    } else if(my_tree.key == "factor NEW INT LBRACK expr RBRACK") {
        string expr = check_type(my_tree.child[3], t, sc);
        if(expr == "int") {
            return "int*";
        } else {
            string error = "ERROR: factor NEW INT LBRACK expr RBRACK";
            throw error;
        }
    } else if(my_tree.key == "factor ID LPAREN RPAREN") {
        bool found = 0;
        if(t.count(my_tree.child[0].lexeme) > 0) {
            found = 1;
        }
        if(found == 0) {
            string error = "ERROR: did not find function";
            throw error;
        }
        if(t[my_tree.child[0].lexeme].first.size() != 0) {
            string error = "ERROR: wrong number of parameters";
            throw error;
        }
        return "int";
    } else if(my_tree.key == "factor ID LPAREN arglist RPAREN") {
        bool found = 0;
        if(t.count(my_tree.child[0].lexeme) > 0) {
            found = 1;
        }
        if(found == 0) {
            string error = "ERROR: did not find function";
            throw error;
        }
        
        Node arglist = Node{my_tree.child[2].key, my_tree.child[2].lexeme, my_tree.child[2].child};
        if(t[my_tree.child[0].lexeme].first.size() == 0) {
            string error = "ERROR: wrong number of parameters";
            throw error;
        }
        int num_func = 0;
        int num_in_wain = 1;
        for(auto each:t[my_tree.child[0].lexeme].first) {
            num_func++;
            if(check_type(arglist.child[0], t, sc) != each) {
                string error = "ERROR: wrong type of parameters";
                throw error;
                break;
            }
            if(arglist.child.size() == 3) {
                arglist = arglist.child[2];
                num_in_wain++;
            } else if(arglist.child.size() == 1) {
                
            }
            
        }

        if(num_func != num_in_wain) {
            string error = "ERROR: wrong number of parameters";
            throw error;
        }
        return "int";
    /*********************** lvalue *****************************/
    } else if(my_tree.key == "lvalue ID") {
        bool found = 0;
        for(auto each:t[sc].second) {
            if(my_tree.child[0].lexeme == each.first) {
                if(each.second == "int*") {
                    found = 1;
                    return "int*";
                } else if(each.second == "int") {
                    found = 1;
                    return "int";
                    
                }
                
            }
        }
        if(found == 0) {
            string error = "ERROR: 404 ID";
            throw error;
        }
    } else if(my_tree.key == "lvalue STAR factor") {
        string fac = check_type(my_tree.child[1], t, sc);
        if(fac == "int*") {
            return "int";
        } else {
            string error = "ERROR: lvalue STAR factor";
            throw error;
        }
    } else if(my_tree.key == "lvalue LPAREN lvalue RPAREN") {
        return check_type(my_tree.child[1], t, sc);

    /************************* statements and statement ***********************/
    } else if(my_tree.key == "statements statements statement") {
        check_type(my_tree.child[0], t, sc);
        check_type(my_tree.child[1], t, sc);
        return "int";
    } else if(my_tree.key == "statement lvalue BECOMES expr SEMI") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        
        if(left == "int" && right == "int") {
            return "int";
        } else if(left == "int*" && right == "int*") {
            return "int";
        } else {
            string error = "ERROR: wrong type of assign";
            throw error;
        } 
    } else if(my_tree.key == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE") {
        check_type(my_tree.child[2], t, sc);
        check_type(my_tree.child[5], t, sc);
        check_type(my_tree.child[9], t, sc);
        return "int";
    } else if(my_tree.key == "statement PRINTLN LPAREN expr RPAREN SEMI") {
        string check_int = check_type(my_tree.child[2], t, sc);
        if(check_int != "int") {
            string error = "ERROR: wrong type for PRINTLN";
            throw error;
        }
        return "int";
    } else if(my_tree.key == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE") {
        check_type(my_tree.child[2], t, sc);
        check_type(my_tree.child[5], t, sc);
        return "int";
    } else if(my_tree.key == "statement DELETE LBRACK RBRACK expr SEMI") {
        string check_int_star = check_type(my_tree.child[3], t, sc);
        if(check_int_star != "int*") {
            string error = "ERROR: wrong type for DELETE";
            throw error;
        }
        return "int";
    } else if(my_tree.key == "statements") {
        return "int";

    /********************************* test *******************************/
    } else if(my_tree.key == "test expr EQ expr" || my_tree.key == "test expr NE expr" || 
              my_tree.key == "test expr LT expr" || my_tree.key == "test expr LE expr" ||
              my_tree.key == "test expr GE expr" || my_tree.key == "test expr GT expr") {
        string left = check_type(my_tree.child[0], t, sc);
        string right = check_type(my_tree.child[2], t, sc);
        if(left == "int" && right == "int*") {
            string error = "ERROR: wrong type for test";
            throw error;
        } else if(left == "int*" && right == "int") {
            string error = "ERROR: wrong type for test";
            throw error;
        } else {
            return "int";
        }
    }
}






void create_table(Node &my_tree, string sc, map<string, pair<vector<string>, map<string, string>>> &t,
                  vector<string> &insert) {
    if(my_tree.key == "procedures procedure procedures") {
        vector<string> vs;
        map<string, string> ssmap;
        for(auto each:t) {
            if(each.first == my_tree.child[0].child[1].lexeme) {
                string error = "ERROR: procedures procedure procedures";
                
                throw error;
            }
        }
        t.insert({my_tree.child[0].child[1].lexeme, {vs, ssmap}});
        insert.push_back(my_tree.child[0].child[1].lexeme);
        create_table(my_tree.child[0], my_tree.child[0].child[1].lexeme, t, insert);
        create_table(my_tree.child[1], sc, t, insert);
    } else if(my_tree.key == "procedures main") {
        vector<string> vs;
        map<string, string> ssmap;
        for(auto each:t) {
            if(each.first == my_tree.child[0].child[1].lexeme) {
                string error = "ERROR: procedures main";
                
                throw error;
            }
        }
        t.insert({my_tree.child[0].child[1].lexeme, {vs, ssmap}});
        insert.push_back(my_tree.child[0].child[1].lexeme);
        create_table(my_tree.child[0], my_tree.child[0].child[1].lexeme, t, insert);
    } else if(my_tree.key == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        
        create_table(my_tree.child[3], sc, t, insert);
        create_table(my_tree.child[6], sc, t, insert);
    } else if(my_tree.key == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        if(my_tree.child[3].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[3].child[1].lexeme == each.first) {
                    string error = "ERROR: Duplicate variable name";
                    throw error;
                }
            }
            t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[3].child[1].lexeme, int_pointer});
        } else if(my_tree.child[3].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[3].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[3].child[1].lexeme, my_int});
        }
        
        

        if(my_tree.child[5].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[5].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT STAR";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[5].child[1].lexeme, int_pointer});
            string error = "ERROR: second parameter of wain cannot be int*";
            throw error;
        } else if(my_tree.child[5].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[5].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[5].child[1].lexeme, my_int});
        }
        create_table(my_tree.child[8], sc, t, insert);
        
        

    /**************************** params and paramlist *********************************/
    } else if(my_tree.key == "params paramlist") {

        create_table(my_tree.child[0], sc, t, insert);

    } else if(my_tree.key == "paramlist dcl") {
        if(my_tree.child[0].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[0].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT STAR";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[0].child[1].lexeme, int_pointer});
        } else if(my_tree.child[0].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[0].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[0].child[1].lexeme, my_int});
        }
        
    } else if(my_tree.key == "paramlist dcl COMMA paramlist") {
        if(my_tree.child[0].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[0].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT STAR";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[0].child[1].lexeme, int_pointer});
        } else if(my_tree.child[0].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[0].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[0].child[1].lexeme, my_int});
        }

        create_table(my_tree.child[2], sc, t, insert);

    /*********************************** dcls ******************************************/
    } else if(my_tree.key == "dcls dcls dcl BECOMES NUM SEMI") {
        create_table(my_tree.child[0], sc, t, insert);
        if(my_tree.child[1].child[0].key == "type INT STAR") {
            string error = "ERROR: wrong type of assignment";
            throw error;
        }

        if(my_tree.child[1].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[1].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT STAR";
                    
                    throw error;
                }
            }
            // t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[1].child[1].lexeme, int_pointer});
        } else if(my_tree.child[1].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[1].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            // t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[1].child[1].lexeme, my_int});
        }
    } else if(my_tree.key == "dcls dcls dcl BECOMES NULL SEMI") {
        create_table(my_tree.child[0], sc, t, insert);
        if(my_tree.child[1].child[0].key == "type INT") {
            string error = "ERROR: wrong type of assignment";
            throw error;
        }

        if(my_tree.child[1].child[0].key == "type INT STAR") {
            string int_pointer = "int*";
            for(auto each:t[sc].second) {
                if(my_tree.child[1].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT STAR";
                    
                    throw error;
                }
            }
            // t[sc].first.push_back(int_pointer);
            t[sc].second.insert({my_tree.child[1].child[1].lexeme, int_pointer});
        } else if(my_tree.child[1].child[0].key == "type INT") {
            string my_int = "int";
            for(auto each:t[sc].second) {
                if(my_tree.child[1].child[1].lexeme == each.first) {
                    string error = "ERROR: type INT";
                    
                    throw error;
                }
            }
            // t[sc].first.push_back(my_int);
            t[sc].second.insert({my_tree.child[1].child[1].lexeme, my_int});
        }

    
    }
}

void print_table(map<string, pair<vector<string>, map<string, string>>> &t, vector<string> insert, map<string, vector<var>> &nt) {
    
    for(int it = 0; it < insert.size(); ++it) {
        
        
        if(it == insert.size() - 1) {
            cerr << insert[it] << " ";
            for(int i = 0; i < t[insert[it]].first.size(); i++) {
                cerr << t[insert[it]].first[i] << " ";
            }
            cerr << endl;
            for(auto itr = t[insert[it]].second.begin(); itr != t[insert[it]].second.end(); ++itr) {
                cerr << itr->first << " " << itr->second << endl;
            }
            
        } else {
            
            cerr << insert[it] << " ";
            for(int i = 0; i < t[insert[it]].first.size(); i++) {
                cerr << t[insert[it]].first[i] << " ";
            }
            cerr << endl;
            for(auto itr = t[insert[it]].second.begin(); itr != t[insert[it]].second.end(); ++itr) {
                cerr << itr->first << " " << itr->second << endl;
            }
            cerr << endl;
        }
    }
}

void push(string reg) {
    cout << "sw " << reg << ", " << "-4" << "($30)" << endl;
    cout << "sub $30, $30, $4" << endl; 
}

void pop(string reg) {
    cout << "add $30, $30, $4" << endl; 
    cout << "lw " << reg << ", -4($30)" << endl;
}

void code_generator(Node &my_tree, string sc, map<string, vector<var>> &nt, vector<string> &insert) {
    if(my_tree.key == "procedures procedure procedures") {
         code_generator(my_tree.child[1], sc, nt, insert);
         code_generator(my_tree.child[0], sc, nt, insert);
    } else if(my_tree.key == "procedures main") {
       
        code_generator(my_tree.child[0], sc, nt, insert);

    } else if(my_tree.key == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        int arg = 0;
        Node tmp = my_tree.child[3];
        if(tmp.key == "params") {
            arg = 0;
        } else if(tmp.key == "params paramlist") {
            tmp = tmp.child[0];
            while(tmp.key != "paramlist dcl") {
                arg++;
                tmp = tmp.child[2];
            }
            arg++;
        }
        vector<var> vvar;
        nt.insert({my_tree.child[1].lexeme, vvar});
        location = 5;
        offset = 0;
        cout << endl;
        cout << "func" << my_tree.child[1].lexeme << ":" << endl;
        cout << "sub $29, $30, $4" << endl;
        
        push("$5");
        push("$6");
        push("$7");
        offset = arg * 4;
        
        code_generator(my_tree.child[3], my_tree.child[1].lexeme, nt, insert);
        while(arg != 0) {
            pop("$5");
            arg--;
        }
        
        offset = -12;
        code_generator(my_tree.child[6], my_tree.child[1].lexeme, nt, insert);
        
        
        code_generator(my_tree.child[7], my_tree.child[1].lexeme, nt, insert);
        code_generator(my_tree.child[9], my_tree.child[1].lexeme, nt, insert);
        pop("$7");
        pop("$6");
        pop("$5");
       
        
        
        cout << "add $30, $29, $4" << endl;
        cout << "jr $31" << endl;



    } else if(my_tree.key == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
        cout << "; begin prologue" << endl;
        cout << ".import print" << endl;
        cout << ".import init" << endl;
        cout << ".import new" << endl;
        cout << ".import delete" <<endl;
        cout << "lis $4" << endl;
        cout << ".word 4" << endl;
        cout << "lis $10" << endl;
        cout << ".word print" << endl;
        cout << "lis $11" << endl;
        cout << ".word 1" << endl;
        cout << "sub $29, $30, $4" << endl;
        location = 1;
        offset = 0;
        vector<var> vvar;
        nt.insert({"wain", vvar});
        if(my_tree.child[3].child[0].child.size() == 2) {
            push("$31");
            cout << "lis $5" << endl;
            cout << ".word init" << endl;
            cout << "jalr $5" << endl;
            pop("$31");
        } else if(my_tree.child[3].child[0].child.size() == 1) {
            push("$2");
            push("$31");
            cout << "add $2, $0, $0" << endl;
            cout << "lis $5" << endl;
            cout << ".word init" << endl;
            cout << "jalr $5" << endl;
            pop("$31");
            pop("$2");
        }
        code_generator(my_tree.child[3], "wain", nt, insert);
        code_generator(my_tree.child[5], "wain", nt, insert);
        code_generator(my_tree.child[8], "wain", nt, insert);
        cout << ";end prologue" << endl;
        cout << endl;
        code_generator(my_tree.child[9], "wain", nt, insert);
        code_generator(my_tree.child[11], "wain", nt, insert);
        for(auto each:nt["wain"]) {
            cout << "add $30, $30, $4" << endl;
        }
        cout << "jr $31" << endl;
    
    /************************************* expr ********************************************/
    } else if(my_tree.key == "expr term") {
        code_generator(my_tree.child[0], sc, nt, insert);
    } else if(my_tree.key == "expr expr PLUS term") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        if(left == "int" && right == "int") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            pop("$5");
            cout << "add $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            cout << "mult $3, $4" << endl;
            cout << "mflo $3" << endl;
            pop("$5");
            cout << "add $3, $5, $3" << endl;
        } else if(left == "int" && right == "int*") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            pop("$5");
            cout << "mult $5, $4" << endl;
            cout << "mflo $5" << endl;
            cout << "add $3, $5, $3" << endl;
        }
        
    } else if(my_tree.key == "expr expr MINUS term") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        if(left == "int" && right == "int") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            pop("$5");
            cout << "sub $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            cout << "mult $3, $4" << endl;
            cout << "mflo $3" << endl;
            pop("$5");
            cout << "sub $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            code_generator(my_tree.child[0], sc, nt, insert);
            push("$3");
            code_generator(my_tree.child[2], sc, nt, insert);
            pop("$5");
            cout << "sub $3, $5, $3" << endl;
            cout << "div $3, $4" << endl;
            cout << "mflo $3" << endl;
        }


    /************************************* term *******************************************/
    } else if(my_tree.key == "term factor") {
        code_generator(my_tree.child[0], sc, nt, insert);
    } else if(my_tree.key == "term term STAR factor") {
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        cout << "mult $5, $3" << endl;
        cout << "mflo $3" << endl;
    } else if(my_tree.key == "term term SLASH factor") {
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        cout << "div $5, $3" << endl;
        cout << "mflo $3" << endl;
    } else if(my_tree.key == "term term PCT factor") {
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        cout << "div $5, $3" << endl;
        cout << "mfhi $3" << endl;


    /*********************************** factor ********************************************/
    } else if(my_tree.key == "factor ID") {
        for(auto each:nt[sc]) {
            
            if(each.name == my_tree.child[0].lexeme && each.location != -1) {
                // cout << "add $3, $" << each.location << ", $0" << endl;
                cout << "lw $3, " << each.offset << "($29)" << endl;
                
                break;
            }
        }
    } else if(my_tree.key == "factor LPAREN expr RPAREN") {
        code_generator(my_tree.child[1], sc, nt, insert);
    } else if(my_tree.key == "factor NUM") {
        cout << "lis $3" << endl;
        cout << ".word " << my_tree.child[0].lexeme << endl;
    } else if(my_tree.key == "factor NULL") {
        cout << "add $3, $0, $11" << endl;
    } else if(my_tree.key == "factor STAR factor") {
        code_generator(my_tree.child[1], sc, nt, insert);
        cout << "lw $3, 0($3)" << endl;

    } else if(my_tree.key == "factor AMP lvalue") {
        Node tmp = my_tree.child[1];
        while(tmp.key == "lvalue LPAREN lvalue RPAREN") {
            tmp = tmp.child[1];
        }
        if(tmp.key == "lvalue ID") {
            cout << "lis $3" << endl;
            for(auto each:nt[sc]) {
            
            if(each.name == tmp.child[0].lexeme && each.location != -1) {
                // cout << "add $3, $" << each.location << ", $0" << endl;
                cout << ".word " << each.offset << endl;
                
                break;
                }
            }
            cout << "add $3, $3, $29" << endl;
        } else {
            code_generator(tmp.child[1], sc, nt, insert);
        }
    } else if(my_tree.key == "factor NEW INT LBRACK expr RBRACK") {
        code_generator(my_tree.child[3], sc, nt, insert);
        cout << "add $1, $3, $0" << endl;
        push("$31");
        cout << "lis $5" << endl;
        cout << ".word new" << endl;
        cout << "jalr $5" << endl;
        pop("$31");
        cout << "bne $3, $0, 1" << endl;
        cout << "add $3, $11, $0" << endl;
    } else if(my_tree.key == "factor ID LPAREN RPAREN") {
        push("$29");
        push("$31");
        cout << "lis $5" << endl;
        cout << ".word " << "func" << my_tree.child[0].lexeme << endl;
        cout << "jalr $5" << endl;
        pop("$31");
        pop("$29");
    } else if(my_tree.key == "factor ID LPAREN arglist RPAREN") {
        int counter = 0;
        push("$29");
        push("$31");
        Node tmp = my_tree.child[2];
        while(tmp.key != "arglist expr") {
            counter++;
            code_generator(tmp.child[0], sc, nt, insert);
            push("$3");
            tmp = tmp.child[2];
        }
        counter++;
        code_generator(tmp.child[0], sc, nt, insert);
        push("$3");
        cout << "lis $5" << endl;
        cout << ".word " << "func" << my_tree.child[0].lexeme <<endl;
        cout << "jalr $5" << endl;
        
        while(counter != 0) {
            pop("$31");
            counter--;
        }

        pop("$31");
        pop("$29");


    /********************************** dcl **********************************************/
    } else if(my_tree.key == "dcl type ID") {
        if(my_tree.child[0].child.size() == 2) {
            nt[sc].push_back(var{my_tree.child[1].lexeme, "int*", location, offset});
            
            string p = "$";
            p.append(to_string(location));
            push(p);
            location++;
            if(location == 3) { // it cannot be 3
                location++;
            }
            if(location == 4) { // it cannot be 4
                location++;
            }
            if(location == 6) {
                location--;
            }
            offset-=4;
        } else {
            nt[sc].push_back(var{my_tree.child[1].lexeme, "int", location, offset});
            
            string p = "$";
            p.append(to_string(location));
            push(p);
            location++;
            if(location == 3) { // it cannot be 3
                location++;
            }
            if(location == 4) { // it cannot be 4
                location++;
            }
            if(location == 6) {
                location--;
            }
            offset-=4;
        }

    /********************************* statements and statement *****************************/
    } else if(my_tree.key == "statements statements statement") {
        code_generator(my_tree.child[0], sc, nt, insert);
        code_generator(my_tree.child[1], sc, nt, insert);
    } else if(my_tree.key == "statement PRINTLN LPAREN expr RPAREN SEMI") {
        push("$1");
        code_generator(my_tree.child[2], sc, nt, insert);
        cout << "add $1, $3, $0" << endl;
        push("$31");
        cout << "jalr $10" << endl;
        pop("$31");
        pop("$1");
    } else if(my_tree.key == "statement lvalue BECOMES expr SEMI") {
        Node tmp = my_tree.child[0];
        while(tmp.key == "lvalue LPAREN lvalue RPAREN") {
            tmp = tmp.child[1];
        }
        
        if(tmp.key == "lvalue STAR factor") {
            code_generator(my_tree.child[2], sc, nt, insert);
            push("$3");
            code_generator(tmp.child[1], sc, nt, insert);
            pop("$5");
            cout << "sw $5, 0($3)" << endl;
            return;
        }
        
        code_generator(my_tree.child[2], sc, nt, insert);
        for(auto each:nt[sc]) {
            if(each.name == tmp.child[0].lexeme && each.location != -1) {
                cout << "sw $3, " << each.offset << "($29)" << endl;
                break;
            }
            
            
            
        }
    } else if(my_tree.key == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE") {
        num_loop++;
        int counter_loop = num_loop;
        cout << "loop" << counter_loop << ":" << endl;
        code_generator(my_tree.child[2], sc, nt, insert);
        cout << "beq $3, $0, endWhile" << counter_loop << endl;
        code_generator(my_tree.child[5], sc, nt, insert);
        cout << "beq $0, $0, loop" << counter_loop << endl;
        cout << "endWhile" << counter_loop << ":" << endl;
    } else if(my_tree.key == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE") {
        num_if++;
        int counter_if = num_if;
        code_generator(my_tree.child[2], sc, nt, insert);
        cout << "beq $3, $0, else" << counter_if << endl;
        code_generator(my_tree.child[5], sc, nt, insert);
        cout << "beq $0, $0, endif" << counter_if << endl;
        cout << "else" << counter_if << ":" << endl;
        code_generator(my_tree.child[9], sc, nt, insert);
        cout << "endif" << counter_if << ":" << endl;
    } else if(my_tree.key == "statement DELETE LBRACK RBRACK expr SEMI") {
        num_delete++;
        int counter_delete = num_delete;
        code_generator(my_tree.child[3], sc, nt, insert);
        cout << "beq $3, $11, skipDelete" << counter_delete << endl;
        cout << "add $1, $3, $0" << endl;
        push("$31");
        cout << "lis $5" << endl;
        cout << ".word delete" << endl;
        cout << "jalr $5" << endl;
        pop("$31");
        cout << "skipDelete" << counter_delete << ":" << endl;

    /******************************** dcls *******************************/
    } else if(my_tree.key == "dcls dcls dcl BECOMES NUM SEMI") {
        code_generator(my_tree.child[0], sc, nt, insert);
        if(my_tree.child[1].child[0].child.size() == 2) {
            nt[sc].push_back(var{my_tree.child[1].child[1].lexeme, "int*", location, offset});
            
        } else {
            nt[sc].push_back(var{my_tree.child[1].child[1].lexeme, "int", location, offset});
            
            
        }
            cout << "lis $" << location << endl;
            cout << ".word " << my_tree.child[3].lexeme << endl;
            string p = "$";
            p.append(to_string(location));
            push(p);
            location++;
            if(location == 3) { // it cannot be 3
                location++;
            }
            if(location == 4) { // it cannot be 4
                location++;
            }
            if(location == 6) {
                location--;
            }

            offset-=4;
    } else if(my_tree.key == "dcls dcls dcl BECOMES NULL SEMI") {
        code_generator(my_tree.child[0], sc, nt, insert);
        if(my_tree.child[1].child[0].child.size() == 2) {
            nt[sc].push_back(var{my_tree.child[1].child[1].lexeme, "int*", location, offset});
        } else {
            nt[sc].push_back(var{my_tree.child[1].child[1].lexeme, "int", location, offset});
        }
        cout << "lis $" << location << endl;
            cout << ".word " << 1 << endl;
            string p = "$";
            p.append(to_string(location));
            push(p);
            location++;
            if(location == 3) { // it cannot be 3
                location++;
            }
            if(location == 4) { // it cannot be 4
                location++;
            }
            if(location == 6) {
                location--;
            }

            offset-=4;
    /*************************** test **********************************/
    } else if(my_tree.key == "test expr LT expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $3, $5, $3" << endl;
        }
        
    } else if(my_tree.key == "test expr GT expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[2], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[0], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $3, $5, $3" << endl;
        }
    } else if(my_tree.key == "test expr NE expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $6, $3, $5" << endl;
            cout << "slt $7, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $6, $3, $5" << endl;
            cout << "sltu $7, $5, $3" << endl;
        }
        cout << "add $3, $6, $7" << endl;
    } else if(my_tree.key == "test expr EQ expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $6, $3, $5" << endl;
            cout << "slt $7, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $6, $3, $5" << endl;
            cout << "sltu $7, $5, $3" << endl;
        }
        cout << "add $3, $6, $7" << endl;
        cout << "sub $3, $11, $3" << endl;
    } else if(my_tree.key == "test expr GE expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[0], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[2], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $3, $5, $3" << endl;
        }
        cout << "sub $3, $11, $3" << endl;
    } else if(my_tree.key == "test expr LE expr") {
        string left = check_type(my_tree.child[0], tables, sc);
        string right = check_type(my_tree.child[2], tables, sc);
        code_generator(my_tree.child[2], sc, nt, insert);
        push("$3");
        code_generator(my_tree.child[0], sc, nt, insert);
        pop("$5");
        if(left == "int" && right == "int") {
            cout << "slt $3, $5, $3" << endl;
        } else if(left == "int*" && right == "int*") {
            cout << "sltu $3, $5, $3" << endl;
        }
        cout << "sub $3, $11, $3" << endl;
    /******************************* lvalue ********************************/
    } else if(my_tree.key == "lvalue STAR factor") {
        code_generator(my_tree.child[1], sc, nt, insert);
    
    /******************************* params *****************************/
    } else if(my_tree.key == "params") {
        return;
    } else if(my_tree.key == "params paramlist") {
        code_generator(my_tree.child[0], sc, nt, insert);

    /****************************** paramlist ***************************/
    } else if(my_tree.key == "paramlist dcl") {
        code_generator(my_tree.child[0], sc, nt, insert);
    } else if(my_tree.key == "paramlist dcl COMMA paramlist") {
        code_generator(my_tree.child[0], sc, nt, insert);
        code_generator(my_tree.child[2], sc, nt, insert);
        
    }
}

int main() {
    vector<Node> TreeStack;
    string line;
    stringstream ss;
    string word;
    ifstream inFile;
    int convert;
    vector<string> terminal;
    vector<string> non_terminal;

    inFile.open("wlp4.txt"); // open the file

      if(!inFile) { // if the file does not exist
          cerr << "Unanble to open the file";
          return -1;
      }

    getline(inFile, line); // read the number of terminals
    convert = stoi(line); // convert the # into int
    // cout << line << endl;
    

    for(int i = 0; i < convert; i++) { // store terminals
        getline(inFile, line);
        
        terminal.push_back(line);
    }

    getline(inFile, line); // read the number of non-terminals
    convert = stoi(line);
    
    for(int i = 0; i < convert; i++) { // store non-terminals
        getline(inFile, line);
        non_terminal.push_back(line);
    }


    getline(cin, line);
    ss << line;
    ss >> word; // ignore start
    while(ss >> word) { // BOF procedures EOF
        vector<Node> n;
        string my_key = word;
        string lex = "";
        Node my_insert = Node{my_key, lex, n};
        TreeStack.push_back(my_insert);
    }
    getline(cin, line); // skip BOF BOF

    create_tree(TreeStack[1], terminal);

    vector<string> insert_order;
    
    map<string, vector<var>> new_table;

    // for(int i = 0; i < TreeStack.size(); i++) {
    //     print_tree(TreeStack[i]);
    // }

    string empty = "";
    
        create_table(TreeStack[1], empty, tables, insert_order);
        check_type(TreeStack[1], tables, empty);
    
    print_table(tables, insert_order, new_table);
    code_generator(TreeStack[1], empty, new_table, insert_order);
    // cerr << endl;
    // for(auto each:new_table) {
    //     cerr << each.first << endl;
    //     for(auto every:each.second) {
    //         cerr << every.name << " " << every.offset << endl;
    //     }
    // }
    return 0;
}


