PAGESIZE := $(shell getconf PAGE_SIZE)

sdir := $(srcdir)/libartbase
odir := $(objdir)/libartbase

ARTBASE_ARCH_SRCS := $(sdir)/arch/instruction_set.cc 
ARTBASE_BASE_SRCS += $(sdir)/base/allocator.cc	$(sdir)/base/arena_allocator.cc $(sdir)/base/arena_bit_vector.cc
ARTBASE_BASE_SRCS += $(sdir)/base/enums.cc $(sdir)/base/file_magic.cc $(sdir)/base/file_utils.cc $(sdir)/base/hex_dump.cc $(sdir)/base/hiddenapi_flags.cc
ARTBASE_BASE_SRCS += $(sdir)/base/malloc_arena_pool.cc $(sdir)/base/membarrier.cc $(sdir)/base/memfd.cc $(sdir)/base/memory_region.cc 
ARTBASE_BASE_SRCS += $(sdir)/base/os_linux.cc $(sdir)/base/runtime_debug.cc $(sdir)/base/safe_copy.cc $(sdir)/base/scoped_arena_allocator.cc
ARTBASE_BASE_SRCS += $(sdir)/base/socket_peer_is_trusted.cc $(sdir)/base/time_utils.cc $(sdir)/base/mem_map.cc
ARTBASE_BASE_SRCS += $(sdir)/base/bit_vector.cc $(sdir)/base/logging.cc $(sdir)/base/scoped_flock.cc 
ARTBASE_BASE_SRCS += $(sdir)/base/utils.cc $(sdir)/base/zip_archive.cc $(sdir)/base/mem_map_unix.cc
ARTBASE_UNIX_SRCS += $(sdir)/base/unix_file/fd_file.cc $(sdir)/base/unix_file/random_access_file_utils.cc



ARTBASE_SRCS := $(ARTBASE_BASE_SRCS) $(ARTBASE_ARCH_SRCS) $(ARTBASE_UNIX_SRCS)
ARTBASE_OBJS := $(patsubst $(sdir)/arch/%.cc,$(odir)/%.op,$(ARTBASE_ARCH_SRCS))
ARTBASE_OBJS += $(patsubst $(sdir)/base/%.cc,$(odir)/%.op,$(ARTBASE_BASE_SRCS))
ARTBASE_OBJS += $(patsubst $(sdir)/base/unix_file/%.cc,$(odir)/%.op,$(ARTBASE_UNIX_SRCS))

ARTBASE_GEN_SRCS := $(sdir)/arch/instruction_set.h
ARTBASE_GEN_SRCS += $(sdir)/base/allocator.h
ARTBASE_GEN_SRCS += $(sdir)/base/unix_file/fd_file.h

ARTBASE_GEND_SRCS := $(sdir)/operator_out.cc
ARTBASE_GEND_OBJS := $(odir)/operator_out.op

_CXXFLAGS := $(CXXFLAGS)
_CXXFLAGS += -I$(LIBZIPARCHIVE_HDRS) -I$(ANDROID_HDRS) -I$(sdir)
_CXXFLAGS += -DPAGE_SIZE=$(PAGESIZE)

all: gensrcs $(odir)/libartbase.op $(ARTBASE_OBJS)

$(ARTBASE_GEND_OBJS): $(ARTBASE_GEND_SRCS)
	$(CXX) $(_CXXFLAGS) -c $^ -o $@

$(odir)/%.op: $(sdir)/arch/%.cc
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/%.op: $(sdir)/base/%.cc
	$(CXX) $(_CXXFLAGS) -c $< -o $@

$(odir)/%.op: $(sdir)/base/unix_file/%.cc
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/libartbase.op: $(ARTBASE_OBJS) $(ARTBASE_GEND_OBJS)
	$(LD) -r -o $@ $^

gensrcs:
	python $(generate_operator_out) $(sdir) $(ARTBASE_GEN_SRCS) > $(ARTBASE_GEND_SRCS)

clean:
	rm -f $(odir)/*.op
	rm -f $(sdir)/operator_out.cc
	rm -f $(odir)/base/*.op $(odir)/arch/*.op $(odir)/base/unix_file/*.op
