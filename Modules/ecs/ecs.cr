module EntityComponentSystem
    class ECS
        struct Property
        #Properties:
        PROPERTY = true
        #Properties:    
        end
        struct Field
        #Fields:
        FIELD = true
        #Fields:    
        end
        struct Tree
        #Tree of Functions:
        def tree_func
        expression = true  
        end
        def tree_func2
        nil    
        end #end of Tree Funcs  
        end #end of Tree Struct
    end #end of ECS class
    
    class Composer < ECS
        #TODO: Add defined properties for I/O
    end #end of Composer Class
end #End of the ECS Module

