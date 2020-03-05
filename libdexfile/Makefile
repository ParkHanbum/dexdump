sdir := $(srcdir)/libdexfile
odir := $(objdir)/libdexfile

LIBDEX_SRCS := $(sdir)/dex/art_dex_file_loader.cc
LIBDEX_SRCS += $(sdir)/dex/compact_dex_file.cc
LIBDEX_SRCS += $(sdir)/dex/compact_offset_table.cc
LIBDEX_SRCS += $(sdir)/dex/descriptors_names.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file_exception_helpers.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file_layout.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file_loader.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file_tracking_registrar.cc
LIBDEX_SRCS += $(sdir)/dex/dex_file_verifier.cc
LIBDEX_SRCS += $(sdir)/dex/dex_instruction.cc
LIBDEX_SRCS += $(sdir)/dex/modifiers.cc
LIBDEX_SRCS += $(sdir)/dex/primitive.cc
LIBDEX_SRCS += $(sdir)/dex/signature.cc
LIBDEX_SRCS += $(sdir)/dex/standard_dex_file.cc
LIBDEX_SRCS += $(sdir)/dex/type_lookup_table.cc
LIBDEX_SRCS += $(sdir)/dex/utf.cc

LIBDEX_OBJS := $(patsubst $(sdir)/dex/%.cc,$(odir)/%.op,$(LIBDEX_SRCS))

_CXXFLAGS := $(CXXFLAGS)
_CXXFLAGS += -I$(sdir) -I$(LIBARTBASE_BASE) -I$(ANDROID_HDRS)
_CXXFLAGS += -I$(LIBARTPALETTE_HDRS) -I$(JNI_HDRS)
_CXXFLAGS += -I$(LIBZIPARCHIVE_HDRS)


all: $(odir)/libdexfile.op $(LIBDEX_OBJS)

$(odir)/%.op: $(sdir)/dex/%.cc
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/libdexfile.op: $(LIBDEX_OBJS)
	$(LD) -r -o $@ $^

clean:
	rm -f $(odir)/*.op
