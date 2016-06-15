class Block(object):
    def __init__(self):
        self.instructions = []
        self.next = None 
    def append(self,instr):
        self.instructions.append(instr)
    def __iter__(self):
        return iter(self.instructions)

class BasicBlock(Block):
    pass

class BlockVisitor(object):
    def visit(self,block):
        while block:
            name = "visit_%s" % type(block).__name__
            if hasattr(self,name):
                getattr(self,name)(block)
            block = getattr(block, "next", None)
